import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

# ── ggplot2 default font ──────────────────────────────────────────────────────
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['Helvetica', 'Arial', 'DejaVu Sans']
FONT = 16

# ── Data ──────────────────────────────────────────────────────────────────────
conditions = [
    'Fit-dom\nrestricted',
    'Fit-dom\nunrestricted',
    'Reference',
    'Soc-dom\nrestricted',
    'Soc-dom\nunrestricted',
]

# Median in_degree
p5_median  = [12044, 32774, 39265,  9661,  8968]
ps5_median = [ 7517, 56297, 59753, 12321, 21557]

# Top 0.1% (%)
p5_top01   = [67, 80, 87, 53, 60]
ps5_top01  = [93, 100, 100, 93, 100]

# Top 0.01% (%)
p5_top001  = [47, 67, 80, 33, 40]
ps5_top001 = [47, 87, 100, 53, 67]

metrics = [
    ('Median in-degree',       p5_median,  ps5_median,  False),
    ('% Agents ≥ p99.9$_{bsl}$', p5_top01,   ps5_top01,   True),
    ('% Agents ≥ p99.99$_{bsl}$',p5_top001,  ps5_top001,  True),
]

# ── Colours ───────────────────────────────────────────────────────────────────
col_p5  = '#2166ac'  # blue
col_ps5 = '#d6604d'  # red-orange
col_line= '#aaaaaa'  # grey connecting line

# ── Figure ────────────────────────────────────────────────────────────────────
fig, axes = plt.subplots(1, 3, figsize=(15, 5.5))

y = np.arange(len(conditions))

for ax, (title, p5_vals, ps5_vals, is_pct) in zip(axes, metrics):
    # Connecting lines
    for i in range(len(conditions)):
        ax.plot([p5_vals[i], ps5_vals[i]], [y[i], y[i]],
                color=col_line, linewidth=1.5, zorder=1)

    # Dots
    ax.scatter(p5_vals,  y, color=col_p5,  s=90, zorder=2, label='P5')
    ax.scatter(ps5_vals, y, color=col_ps5, s=90, zorder=2, label='PS5')

    # Axes
    ax.set_yticks(y)
    if ax is axes[0]:
        ax.set_yticklabels(conditions, fontsize=FONT - 1)
    else:
        ax.set_yticklabels([])
    ax.set_xlabel(title, fontsize=FONT)
    ax.tick_params(axis='x', labelsize=FONT - 2)

    if is_pct:
        ax.set_xlim(25, 110)
        ax.axvline(100, color='#cccccc', linewidth=0.8, linestyle='--')
    else:
        ax.xaxis.set_major_formatter(
            matplotlib.ticker.FuncFormatter(lambda x, _: f'{int(x/1000)}k' if x >= 1000 else str(int(x)))
        )

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.grid(axis='x', linestyle=':', linewidth=0.7, alpha=0.7)

# Legend on first panel only
p5_patch  = mpatches.Patch(color=col_p5,  label='P5 (simultaneous)')
ps5_patch = mpatches.Patch(color=col_ps5, label='PS5 (staggered)')
axes[0].legend(handles=[p5_patch, ps5_patch], fontsize=FONT - 2,
               loc='lower right', frameon=False)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/p5_ps5_comparison.pdf',
            bbox_inches='tight', dpi=300)
plt.savefig('/mnt/user-data/outputs/p5_ps5_comparison.png',
            bbox_inches='tight', dpi=300)
print("Saved.")
