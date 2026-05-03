import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker

# ── ggplot2 default font ──────────────────────────────────────────────────────
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['Helvetica', 'Arial', 'DejaVu Sans']
FONT = 16

# ── Data ──────────────────────────────────────────────────────────────────────
conditions = [
    'A0: Reference',
    'A1: Remove PA wt.',
    'A2: Remove fit. wt.',
    'A3: Remove NA wt.',
    'A4: Remove AR wt.',
    'A5: Ablate fitness',
    'A6: Ablate NA',
    'A7: Ablate OD',
]

p5_median  = [13441, 14611, 22385, 22487, 44353,  5062, 16694,  5349]
ps5_median = [33137, 28624, 31306, 52596, 65672,  3546, 17524, 17013]

p5_top01   = [100, 100, 100, 100, 100,  80,  93,  80]
ps5_top01  = [100, 100, 100, 100, 100,  73, 100, 100]

p5_top001  = [ 80,  87,  93,  80,  67,  33,  73,  40]
ps5_top001 = [ 87,  87,  80,  93, 100,  33,  73,  80]

metrics = [
    ('Median in-degree',            p5_median,  ps5_median,  False),
    ('% Agents ≥ p99.9$_{bsl}$',   p5_top01,   ps5_top01,   True),
    ('% Agents ≥ p99.99$_{bsl}$',  p5_top001,  ps5_top001,  True),
]

# ── Colours ───────────────────────────────────────────────────────────────────
col_p5   = '#2166ac'
col_ps5  = '#d6604d'
col_line = '#aaaaaa'

# ── Figure ────────────────────────────────────────────────────────────────────
fig, axes = plt.subplots(1, 3, figsize=(16, 6))

y = np.arange(len(conditions))

for ax_idx, (ax, (title, p5_vals, ps5_vals, is_pct)) in enumerate(zip(axes, metrics)):
    # Connecting lines
    for i in range(len(conditions)):
        ax.plot([p5_vals[i], ps5_vals[i]], [y[i], y[i]],
                color=col_line, linewidth=1.5, zorder=1)

    # Dots
    ax.scatter(p5_vals,  y, color=col_p5,  s=80, zorder=2, label='P5')
    ax.scatter(ps5_vals, y, color=col_ps5, s=80, zorder=2, label='PS5')

    # Y axis — only label left panel
    ax.set_yticks(y)
    if ax_idx == 0:
        ax.set_yticklabels(conditions, fontsize=FONT - 2)
    else:
        ax.set_yticklabels([])

    ax.set_xlabel(title, fontsize=FONT)
    ax.tick_params(axis='x', labelsize=FONT - 2)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.grid(axis='x', linestyle=':', linewidth=0.7, alpha=0.7)

    if is_pct:
        ax.set_xlim(25, 110)
        ax.axvline(100, color='#cccccc', linewidth=0.8, linestyle='--')
    else:
        ax.xaxis.set_major_formatter(
            matplotlib.ticker.FuncFormatter(
                lambda val, _: f'{int(val/1000)}k' if val >= 1000 else str(int(val))
            )
        )

# Legend on center panel
p5_patch  = mpatches.Patch(color=col_p5,  label='P5 (simultaneous)')
ps5_patch = mpatches.Patch(color=col_ps5, label='PS5 (staggered)')
axes[1].legend(handles=[p5_patch, ps5_patch], fontsize=FONT - 2,
               loc='lower left', frameon=False)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/ablation_p5_ps5_dots.pdf',
            bbox_inches='tight', dpi=300)
plt.savefig('/mnt/user-data/outputs/ablation_p5_ps5_dots.png',
            bbox_inches='tight', dpi=300)
print("Saved.")
