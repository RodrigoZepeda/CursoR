"""
Estilo y utilidades compartidas por los dos scripts que generan las figuras
de la presentacion "Regresion, geometricamente".

Paleta de alto contraste apta para proyector y para daltonismo.
"""

import os

import matplotlib.pyplot as plt

COLOR = {
    "blue": "#2a78d6",
    "orange": "#eb6834",
    "aqua": "#1baf7a",
    "yellow": "#eda100",
    "magenta": "#e87ba4",
    "green": "#008300",
    "violet": "#4a3aa7",
    "red": "#e34948",
}
INK = "#0b0b0b"          # texto principal
INK_2 = "#52514e"        # texto secundario
MUTED = "#898781"        # ejes / ticks
GRID = "#e1e0d9"         # lineas de cuadricula
AXIS = "#c3c2b7"         # spines
SURFACE = "#fcfcfb"      # fondo de la figura

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Arial", "DejaVu Sans"],
    "figure.facecolor": SURFACE,
    "savefig.facecolor": SURFACE,
    "axes.facecolor": SURFACE,
    "text.color": INK,
    "mathtext.fontset": "cm",
})

HERE = os.path.dirname(os.path.abspath(__file__))
OUTDIR = os.path.join(HERE, "figuras")
os.makedirs(OUTDIR, exist_ok=True)

SLIDE_SIZE = (12, 6.75)   # 16:9, para insertar directo en PowerPoint


def save(fig, name):
    path = os.path.join(OUTDIR, name)
    fig.savefig(path, format="pdf", bbox_inches="tight", pad_inches=0.25)
    plt.close(fig)
    print(f"  guardado: {path}")


def style_2d(ax, xlabel, ylabel, title=None):
    ax.set_facecolor(SURFACE)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.spines["left"].set_color(AXIS)
    ax.spines["bottom"].set_color(AXIS)
    ax.spines["left"].set_linewidth(1.1)
    ax.spines["bottom"].set_linewidth(1.1)
    ax.tick_params(colors=MUTED, labelsize=10.5)
    ax.grid(True, color=GRID, linewidth=0.8, zorder=0)
    ax.set_axisbelow(True)
    ax.set_xlabel(xlabel, fontsize=12.5, color=INK, labelpad=8)
    ax.set_ylabel(ylabel, fontsize=12.5, color=INK, labelpad=8)
    if title:
        ax.set_title(title, fontsize=16, color=INK, fontweight="bold",
                      pad=18, loc="left")


def style_3d(ax, xlabel, ylabel, zlabel, title=None, elev=22, azim=-55):
    ax.set_facecolor(SURFACE)
    for axis in (ax.xaxis, ax.yaxis, ax.zaxis):
        axis.pane.set_facecolor(SURFACE)
        axis.pane.set_edgecolor(GRID)
        axis.pane.set_alpha(1.0)
        axis._axinfo["grid"]["color"] = GRID
        axis._axinfo["grid"]["linewidth"] = 0.6
    ax.set_xlabel(xlabel, fontsize=10.5, color=INK, labelpad=10)
    ax.set_ylabel(ylabel, fontsize=10.5, color=INK, labelpad=10)
    ax.set_zlabel(zlabel, fontsize=10.5, color=INK, labelpad=6)
    ax.tick_params(colors=MUTED, labelsize=8.5)
    if title:
        ax.set_title(title, fontsize=15, color=INK, fontweight="bold", pad=2)
    ax.view_init(elev=elev, azim=azim)


def slope_triangle(ax, x_a, x_b, f, color=INK_2, label_dx="", label_dy=""):
    y_a, y_b = f(x_a), f(x_b)
    ax.plot([x_a, x_b], [y_a, y_a], linestyle=(0, (4, 3)), color=color, linewidth=1.6, zorder=3)
    ax.plot([x_b, x_b], [y_a, y_b], linestyle=(0, (4, 3)), color=color, linewidth=1.6, zorder=3)
    ax.plot([x_a, x_b], [y_a, y_b], "o", color=color, markersize=6, zorder=4)
    ax.annotate(label_dx, xy=((x_a + x_b) / 2, y_a), xytext=(0, -20),
                textcoords="offset points", ha="center", fontsize=11.5, color=color)
    ax.annotate(label_dy, xy=(x_b, (y_a + y_b) / 2), xytext=(12, 0),
                textcoords="offset points", va="center", fontsize=11.5, color=color)


def style_mini(ax):
    """Estilo compacto para paneles pequeños (p.ej. cuadrículas 4x4)."""
    ax.set_facecolor(SURFACE)
    for spine in ("top", "right"):
        ax.spines[spine].set_visible(False)
    for spine in ("left", "bottom"):
        ax.spines[spine].set_color(AXIS)
        ax.spines[spine].set_linewidth(0.8)
    ax.tick_params(colors=MUTED, labelsize=7.5, length=2)
    ax.grid(True, color=GRID, linewidth=0.5, zorder=0)
    ax.set_axisbelow(True)


def eq_box(ax, text, loc="lower right", fontsize=13):
    """Caja con la ecuacion del modelo, estilo consistente en toda la baraja."""
    positions = {
        "lower right": dict(x=0.985, y=0.04, ha="right", va="bottom"),
        "lower left": dict(x=0.02, y=0.04, ha="left", va="bottom"),
        "upper right": dict(x=0.985, y=0.94, ha="right", va="top"),
        "upper left": dict(x=0.02, y=0.94, ha="left", va="top"),
    }
    p = positions[loc]
    ax.text(p["x"], p["y"], text, transform=ax.transAxes, ha=p["ha"], va=p["va"],
            fontsize=fontsize, color=INK, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.35", facecolor="white",
                      edgecolor=AXIS, linewidth=1))
