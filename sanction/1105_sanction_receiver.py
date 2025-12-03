# ==========================================
# Interactive Sanctions Map (Sender vs Target)
# ==========================================

import pandas as pd
import plotly.express as px

# 1. Load and clean data
df = pd.read_csv("GSDB_V4.csv")

# Replace ongoing sanctions (coded as 2023) with the current year (2025)
df["end"] = df["end"].replace(2023, 2025)


# Expand sanctions over their active years
df_expanded = df.loc[df.index.repeat(df["end"] - df["begin"] + 1)].copy()
df_expanded["year"] = df_expanded.groupby(level=0).cumcount() + df_expanded["begin"]

# ===============================
# 2. Aggregate active sanctions
# ===============================

# By target (receiver)
agg_target = (
    df_expanded.groupby(["sanctioned_state", "year"])
    .size()
    .reset_index(name="active_sanctions")
)
agg_target["type"] = "Target"


# Combine both datasets
agg = pd.concat([agg_target], ignore_index=True)
agg.rename(columns={"sanctioned_state": "country"}, inplace=True)
agg["year"] = agg["year"].astype(int)

# Sort years for proper chronological animation
agg = agg.sort_values(by="year")

# ===============================
# 3. Create interactive map
# ===============================
fig = px.choropleth(
    agg,
    locations="country",
    locationmode="country names",
    color="active_sanctions",
    hover_name="country",
    animation_frame="year",
    facet_col="type",
    color_continuous_scale="Reds",
    title="Global Economic Sanctions: Receivers (1950–2025)",
)

# Ensure time is ordered correctly
fig.layout.sliders[0]["active"] = 0
fig.layout.sliders[0]["steps"] = sorted(fig.layout.sliders[0]["steps"], key=lambda x: int(x["label"]))

# ===============================
# 4. Add dropdown menu for view toggle
# ===============================
fig.update_layout(
    updatemenus=[
        dict(
            buttons=list([
                dict(
                    args=[{"visible": [True if t == "Target" else False for t in agg["type"]]}],
                    label="Targets (Sanctioned States)",
                    method="update",
                ),
                dict(
                    args=[{"visible": [True if t == "Sender" else False for t in agg["type"]]}],
                    label="Senders (Sanctioning States)",
                    method="update",
                ),
                dict(
                    args=[{"visible": [True] * len(agg)}],
                    label="Show Both",
                    method="update",
                ),
            ]),
            direction="down",
            showactive=True,
            x=0.17,
            xanchor="left",
            y=1.15,
            yanchor="top",
        )
    ],
    geo=dict(showframe=False, showcoastlines=False, projection_type="natural earth"),
    coloraxis_colorbar=dict(title="Active Sanctions"),
    title_x=0.45,
)

# ===============================
# 5. Export to HTML
# ===============================
fig.write_html("sanctions_receiver_map.html", include_plotlyjs="cdn")

print("✅ Interactive sanctions map saved as 'sanctions_sender_receiver_map.html'.")
