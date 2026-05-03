"""
SASCA-ReS-A Validation Script (Python)
Baseline characterization + planted node analysis
Works for: baseline runs, p5, ps5 experiments

Dependencies: pandas, numpy, scipy, matplotlib, seaborn
Install if needed:
    pip install pandas numpy scipy matplotlib seaborn
"""

import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")

# ============================================================
# CONFIGURATION — edit these paths before running
# ============================================================

BASELINE_FILES = {
    "bsl1": "bsl1_mar12026.csv",
    "bsl2": "bsl2_mar12026.csv",
    "bsl3": "bsl3_mar12026.csv",
}

# Planted experiment files — fill in when available
# P5_FILES  = {"p5_r1": "p5_r1_nodelist.csv", ...}
# PS5_FILES = {"ps5_r1": "ps5_r1_nodelist.csv", ...}

# Planted node IDs — fill in when known from your config
# P5_PLANTED_IDS  = [id1, id2, id3, id4, id5]
# PS5_PLANTED_IDS = [id1, id2, id3, id4, id5]

COLS_KEEP = [
    "node_id", "type", "year", "in_degree",
    "fit_peak_value", "num_authors", "out_degree", "alpha",
    "pa_weight", "fit_weight", "num_authors_weight",
    "author_reputation_weight", "sampled_neighborhood_size",
    "planted_nodes_line_number"
]

# ============================================================
# HELPER FUNCTIONS
# ============================================================

def read_nodelist(path, run_label=None):
    """Read nodelist CSV, filter to agents, return key columns."""
    print(f"Reading: {path}")
    df = pd.read_csv(path, usecols=COLS_KEEP)
    df = df[df["type"] == "agent"].copy()
    if run_label:
        df["run"] = run_label
    return df


def add_ranks(df):
    """Add rank column: rank 1 = highest in_degree. Ties by min method."""
    df = df.copy()
    df["rank"] = df["in_degree"].rank(ascending=False, method="min").astype(int)
    return df


def rank_summary(ranks, label=""):
    """Compute summary metrics matching the ablation table format."""
    ranks = np.array(ranks)
    return {
        "condition":   label,
        "n":           len(ranks),
        "median_rank": np.median(ranks),
        "mean_rank":   np.mean(ranks),
        "sd_rank":     np.std(ranks, ddof=1),
        "top10":       int(np.sum(ranks <= 10)),
        "top100":      int(np.sum(ranks <= 100)),
        "worst_rank":  int(np.max(ranks)),
    }


def rank_planted_nodes(df_ranked, planted_ids):
    """Extract planted node rows with their ranks."""
    return df_ranked[df_ranked["node_id"].isin(planted_ids)][
        ["node_id", "in_degree", "rank", "run"]
    ].copy()


# ============================================================
# SECTION 1: LOAD BASELINE DATA
# ============================================================

print("\n=== Loading baseline runs ===")
baseline_dfs = {}
for label, path in BASELINE_FILES.items():
    df = read_nodelist(path, run_label=label)
    baseline_dfs[label] = add_ranks(df)

baseline_all = pd.concat(baseline_dfs.values(), ignore_index=True)

# ============================================================
# SECTION 2: BASELINE IN-DEGREE DISTRIBUTION
# ============================================================

print("\n=== In-degree distribution (baseline) ===")

baseline_stats = baseline_all.groupby("run")["in_degree"].agg(
    n_agents="count",
    mean_indeg="mean",
    median_indeg="median",
    sd_indeg="std",
    max_indeg="max",
).assign(
    p90=lambda x: baseline_all.groupby("run")["in_degree"]
        .quantile(0.90).values,
    p99=lambda x: baseline_all.groupby("run")["in_degree"]
        .quantile(0.99).values,
    n_zero=lambda x: baseline_all.groupby("run")["in_degree"]
        .apply(lambda s: (s == 0).sum()).values
)
print(baseline_stats.to_string())

# Log-log in-degree distribution plot
fig, ax = plt.subplots(figsize=(8, 5))
colors = {"bsl1": "#1f77b4", "bsl2": "#ff7f0e", "bsl3": "#2ca02c"}

for run, df in baseline_dfs.items():
    indeg = df[df["in_degree"] > 0]["in_degree"]
    counts = indeg.value_counts().sort_index()
    ax.scatter(counts.index, counts.values,
               alpha=0.4, s=4, label=run, color=colors.get(run))

ax.set_xscale("log")
ax.set_yscale("log")
ax.set_xlabel("In-degree (log scale)")
ax.set_ylabel("Count (log scale)")
ax.set_title("Baseline In-degree Distribution (3 runs)")
ax.legend()
ax.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{int(x):,}"))
ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{int(x):,}"))
plt.tight_layout()
plt.savefig("baseline_indegree_loglog.pdf")
plt.close()
print("Saved: baseline_indegree_loglog.pdf")

# ============================================================
# SECTION 3: BASELINE RANK DISTRIBUTION SUMMARY
# ============================================================

print("\n=== Rank distribution summary (baseline agents) ===")

summaries = []
for run, df in baseline_dfs.items():
    summaries.append(rank_summary(df["rank"].values, label=run))

summary_df = pd.DataFrame(summaries)
print(summary_df.to_string(index=False))

# ============================================================
# SECTION 4: CROSS-RUN CONSISTENCY (SPEARMAN ON QUANTILES)
# ============================================================

print("\n=== Cross-run Spearman correlations (in-degree quantiles) ===")

q_seq = np.arange(0.01, 1.00, 0.01)
quantile_df = pd.DataFrame({
    run: df["in_degree"].quantile(q_seq).values
    for run, df in baseline_dfs.items()
})

cor_matrix = quantile_df.corr(method="spearman")
print("Spearman correlation matrix of in-degree quantiles across runs:")
print(cor_matrix.round(4).to_string())

# ============================================================
# SECTION 5: TOP-RANKED AGENT CHARACTERIZATION
# ============================================================

print("\n=== Top 100 agents: parameter profiles ===")

param_cols = [
    "fit_peak_value", "num_authors", "out_degree", "alpha",
    "pa_weight", "fit_weight", "num_authors_weight",
    "author_reputation_weight", "sampled_neighborhood_size"
]

top100 = baseline_all[baseline_all["rank"] <= 100]
top100_summary = top100.groupby("run")[param_cols].mean()
print("Mean parameters of top-100 agents per run:")
print(top100_summary.to_string())

print("\nTop-100 vs all agents (pooled across runs):")
compare = pd.concat([
    baseline_all[param_cols].assign(group="all_agents"),
    top100[param_cols].assign(group="top100")
]).groupby("group")[param_cols].agg(["mean", "median"])
print(compare.to_string())

# ============================================================
# SECTION 6: NEIGHBORHOOD SIZE vs RANK (SPEARMAN)
# ============================================================

print("\n=== Neighborhood size vs rank: Spearman r ===")

for run, df in baseline_dfs.items():
    sub = df[(df["sampled_neighborhood_size"] > 0) &
             (df["sampled_neighborhood_size"].notna())]
    r, p = stats.spearmanr(sub["sampled_neighborhood_size"], sub["rank"])
    print(f"  {run}: Spearman r = {r:.4f}, p = {p:.2e}, n = {len(sub)}")

# Log-log scatter: neighborhood size vs in_degree (sample 10k per run)
fig, ax = plt.subplots(figsize=(8, 5))
rng = np.random.default_rng(42)

for run, df in baseline_dfs.items():
    sub = df[(df["sampled_neighborhood_size"] > 0) & (df["in_degree"] > 0)]
    sample = sub.sample(n=min(10000, len(sub)), random_state=42)
    ax.scatter(sample["sampled_neighborhood_size"], sample["in_degree"],
               alpha=0.2, s=2, label=run, color=colors.get(run))

ax.set_xscale("log")
ax.set_yscale("log")
ax.set_xlabel("Sampled neighborhood size (log)")
ax.set_ylabel("In-degree (log)")
ax.set_title("Neighborhood Size vs In-degree (baseline, sampled)")
ax.legend()
plt.tight_layout()
plt.savefig("baseline_nbhd_vs_indegree.pdf")
plt.close()
print("Saved: baseline_nbhd_vs_indegree.pdf")

# ============================================================
# SECTION 7: PLANTED NODE ANALYSIS (uncomment when files ready)
# ============================================================

# --- p5 analysis ---
# p5_dfs = {}
# for label, path in P5_FILES.items():
#     df = read_nodelist(path, run_label=label)
#     p5_dfs[label] = add_ranks(df)
#
# p5_planted_rows = pd.concat([
#     rank_planted_nodes(df, P5_PLANTED_IDS)
#     for df in p5_dfs.values()
# ])
# p5_summary = rank_summary(p5_planted_rows["rank"].values, label="p5")
# print(pd.DataFrame([p5_summary]).to_string(index=False))

# --- ps5 analysis ---
# ps5_dfs = {}
# for label, path in PS5_FILES.items():
#     df = read_nodelist(path, run_label=label)
#     ps5_dfs[label] = add_ranks(df)
#
# ps5_planted_rows = pd.concat([
#     rank_planted_nodes(df, PS5_PLANTED_IDS)
#     for df in ps5_dfs.values()
# ])
# ps5_summary = rank_summary(ps5_planted_rows["rank"].values, label="ps5")
# print(pd.DataFrame([ps5_summary]).to_string(index=False))

# --- Comparison table ---
# comparison = pd.DataFrame([p5_summary, ps5_summary])
# print("\nComparison: p5 vs ps5")
# print(comparison.to_string(index=False))

# --- Plot: planted ranks vs baseline distribution ---
# fig, ax = plt.subplots(figsize=(6, 5))
# for condition, planted_rows in [("p5", p5_planted_rows), ("ps5", ps5_planted_rows)]:
#     ax.scatter(
#         [condition] * len(planted_rows),
#         planted_rows["rank"],
#         alpha=0.7, s=60, label=condition
#     )
# ax.set_yscale("log")
# ax.set_ylabel("Rank (log scale)")
# ax.set_title("Planted Node Ranks: p5 vs ps5")
# ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{int(x):,}"))
# plt.tight_layout()
# plt.savefig("planted_ranks_p5_vs_ps5.pdf")
# plt.close()

print("\n=== Done ===")
