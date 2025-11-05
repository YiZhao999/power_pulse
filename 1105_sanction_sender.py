# ==========================================
# Interactive Sanctions Map (Sender vs Target)
# ==========================================

import pandas as pd
import plotly.express as px


df = pd.read_csv("GSDB_V4.csv")


def clean_sender_names(df):
    """Split, normalize, and standardize sanctioning_state column."""
    # Split multi-country entries
    df = df.assign(sanctioning_state=df["sanctioning_state"].str.split("[,;]"))
    df = df.explode("sanctioning_state")
    df["sanctioning_state"] = df["sanctioning_state"].str.strip()

    # Drop missing
    df = df[df["sanctioning_state"].notna() & (df["sanctioning_state"] != "")]

    # Replace organizations & historical entities
    replacements = {
        "EU": "European Union",
        "EEC": "European Union",
        "ECOWAS": "Nigeria",
        "African Union": "Ethiopia",
        "Organisation of African Unity": "Ethiopia",
        "League of Arab States": "Egypt, Arab Rep.",
        "Commonwealth": "United Kingdom",
        "NATO": "United States",
        "UN": "United States",
        "CSCE": "Switzerland",
        "OSCE": "Switzerland",
        "OAPEC": "Saudi Arabia",
        "OIC": "Saudi Arabia",
        "CoCom": "United States",
        "ChinCom": "China",
        "NAFTA": "United States",
        "MERCOSUR": "Brazil",
        "SADC": "South Africa",
        "UNASUR": "Brazil",
        "Pacific Islands Forum": "Australia",
        "Organization of American States": "United States",
        "Organization of Eastern Carribean States": "Saint Lucia",
        # Historical states
        "Soviet Union": "Russia",
        "German Democratic Republic": "Germany",
        "South Vietnam": "Vietnam",
        "FRY": "Serbia",
    }
    df["sanctioning_state"] = df["sanctioning_state"].replace(replacements)

    # Remove bad descriptive entries
    bad_patterns = ["Agreement", "Participants", "Signatories", "Process", "G8", "G7"]
    mask = df["sanctioning_state"].apply(lambda x: not any(p in x for p in bad_patterns))
    df = df[mask]

    # Fix incomplete or ambiguous fragments
    fix_map = {
        "Arab Rep.": "Egypt, Arab Rep.",
        "Democratic Republic of the": "Democratic Republic of the Congo",
        "Ethiopia (excludes Eritrea)": "Ethiopia",
        "North": "North Korea",
        "South": "South Korea",
        "Malaya": "Malaysia",
        "The": None,
        "European Union": "Belgium",  # map EU to Brussels for visualization
    }
    df["sanctioning_state"] = df["sanctioning_state"].replace(fix_map)

    # Drop any remaining missing
    df = df[df["sanctioning_state"].notna() & (df["sanctioning_state"] != "")]
    return df


df = clean_sender_names(df)
df["end"] = df["end"].replace(2023, 2025)

# 🔒 Reconfirm clean year bounds before expansion
df = df.reset_index(drop=True)
# --- Safe expansion block ---
df = df.reset_index(drop=True)  # ensure unique, sequential index
df["duration"] = (df["end"] - df["begin"] + 1).astype(int)

# Sanity check: should all be reasonable (0–80 years)
print("Max duration:", df["duration"].max())

# Expand rows for each active year
df_expanded = df.loc[df.index.repeat(df["duration"])].copy()
df_expanded["year"] = df_expanded.groupby(level=0).cumcount() + df_expanded["begin"]

print("Expanded year range:", df_expanded["year"].min(), "-", df_expanded["year"].max())



# ===============================
# 2. Aggregate active sanctions
# ===============================

# By target (receiver)
agg_sender = (
    df_expanded.groupby(["sanctioning_state", "year"])
    .size()
    .reset_index(name="active_sanctions")
)
agg_sender["type"] = "Sender"


# Combine both datasets
agg = pd.concat([agg_sender], ignore_index=True)
agg.rename(columns={"sanctioning_state": "country"}, inplace=True)
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
    title="Global Economic Sanctions: Senders (1950–2025)",
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
fig.write_html("sanctions_sender_map.html", include_plotlyjs="cdn")

