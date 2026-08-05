"""
Parte 1 de 2 -- Genera las figuras geometricas "clasicas" para la presentacion
"Regresion, geometricamente" dirigida a personal de salud publica: linea,
pendiente, intercepto, planos, interaccion y multinivel.

Corre con:
    python3 generar_figuras_regresion.py

La parte 2 (residuales, Poisson, logistica, series de tiempo) esta en
generar_figuras_glm_series.py.

Historia de variables (consistente en las dos partes):
    y  = Presion arterial sistolica, PAS (mmHg)
    x1 = Edad (anios)
    x2 = Indice de masa corporal, IMC (kg/m2)
    g  = Sexo (Hombres / Mujeres)
"""

import numpy as np
from matplotlib.gridspec import GridSpec
from matplotlib.patches import Patch

import matplotlib.pyplot as plt

from estilo_figuras import (
    COLOR, INK, INK_2, MUTED, GRID, AXIS, SURFACE, SLIDE_SIZE,
    save, style_2d, style_3d, slope_triangle, eq_box,
)


# ==========================================================================
# Diapositiva 1 -- anatomia de una linea: pendiente e intercepto
# ==========================================================================
def slide_01():
    beta0, beta1 = 100.0, 0.5
    f = lambda x: beta0 + beta1 * x

    x = np.linspace(-3, 92, 400)
    obs_lo, obs_hi = 20, 80

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)

    ax.axvspan(obs_lo, obs_hi, color=COLOR["blue"], alpha=0.06, zorder=0)
    ax.text((obs_lo + obs_hi) / 2, 165, "rango de edades observado en los pacientes",
            ha="center", fontsize=10.5, color=INK_2, style="italic")

    x_extra = x[x < obs_lo]
    x_obs = x[(x >= obs_lo)]
    ax.plot(x_extra, f(x_extra), color=COLOR["blue"], linewidth=2.6,
            linestyle=(0, (1, 2.2)), alpha=0.55, zorder=2)
    ax.plot(x_obs, f(x_obs), color=COLOR["blue"], linewidth=3.2, zorder=2)

    ax.plot([0], [beta0], "o", color=COLOR["red"], markersize=9, zorder=5)
    ax.plot([0, 0], [f(-3) - 10, beta0], color=COLOR["red"], linewidth=1.4,
            linestyle=(0, (4, 3)), zorder=1)
    ax.annotate(
        f"Intercepto  $\\beta_0$ = {beta0:.0f} mmHg\n(PAS teórica a Edad = 0)",
        xy=(0, beta0), xytext=(8, 60), textcoords="offset points",
        fontsize=11.5, color=COLOR["red"],
        arrowprops=dict(arrowstyle="-", color=COLOR["red"], lw=1.2),
    )

    x_a, x_b = 22, 52
    slope_triangle(ax, x_a, x_b, f, color=COLOR["orange"],
                    label_dx="$\\Delta$Edad = 30 años", label_dy="$\\Delta$PAS = 15 mmHg")
    ax.annotate(
        "Pendiente  $\\beta_1$ = $\\dfrac{\\Delta PAS}{\\Delta Edad}$ = $\\dfrac{15}{30}$ = 0.50 mmHg/año",
        xy=(x_b, f(x_b)), xytext=(20, -55), textcoords="offset points",
        fontsize=12.5, color=COLOR["orange"], fontweight="bold",
    )

    ax.set_xlim(-5, 92)
    ax.set_ylim(90, 172)
    style_2d(ax, "Edad (años)", "Presión arterial sistólica, PAS (mmHg)",
             title="Anatomía de una línea de regresión")
    eq_box(ax, "PAS = $\\beta_0$ + $\\beta_1 \\cdot$ Edad")
    save(fig, "01_linea_pendiente_intercepto.pdf")


# ==========================================================================
# Diapositiva 2 -- mismo pendiente, distinto intercepto
# ==========================================================================
def slide_02():
    beta1 = 0.5
    intercepts = [88, 104, 120]
    colors = [COLOR["blue"], COLOR["orange"], COLOR["aqua"]]
    labels = ["Escenario A", "Escenario B", "Escenario C"]

    x = np.linspace(0, 85, 300)
    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.axvspan(20, 80, color=INK, alpha=0.035, zorder=0)

    for b0, c, lab in zip(intercepts, colors, labels):
        y = b0 + beta1 * x
        ax.plot(x, y, color=c, linewidth=3, zorder=3,
                label=f"{lab}:  $\\beta_0$={b0}, $\\beta_1$={beta1}")
        ax.plot([0], [b0], "o", color=c, markersize=8, zorder=4)

    ax.annotate("Las 3 líneas suben igual de rápido\n(misma pendiente $\\beta_1$ = 0.50 mmHg/año)",
                xy=(60, 104 + beta1 * 60), xytext=(-190, 40), textcoords="offset points",
                fontsize=11.5, color=INK_2,
                arrowprops=dict(arrowstyle="-", color=INK_2, lw=1.1))

    ax.set_xlim(-3, 85)
    ax.set_ylim(80, 170)
    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="Mismo efecto de la edad, distinto punto de partida")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11.5)
    for text in leg.get_texts():
        text.set_color(INK)
    eq_box(ax, "PAS = $\\beta_0$ + 0.50 $\\cdot$ Edad")
    save(fig, "02_mismo_pendiente_distinto_intercepto.pdf")


# ==========================================================================
# Diapositiva 3 -- mismo intercepto, distinto pendiente
# ==========================================================================
def slide_03():
    beta0 = 60
    slopes = [0.2, 0.5, 0.9]
    colors = [COLOR["blue"], COLOR["orange"], COLOR["aqua"]]

    x = np.linspace(0, 80, 300)
    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.axvspan(20, 80, color=INK, alpha=0.035, zorder=0)

    for b1, c in zip(slopes, colors):
        y = beta0 + b1 * x
        ax.plot(x, y, color=c, linewidth=3, zorder=3,
                label=f"$\\beta_1$ = {b1:.1f} mmHg/año")

    ax.plot([0], [beta0], "o", color=INK, markersize=10, zorder=5)
    ax.annotate(f"Intercepto común\n$\\beta_0$ = {beta0} mmHg",
                xy=(0, beta0), xytext=(20, -45), textcoords="offset points",
                fontsize=11.5, color=INK,
                arrowprops=dict(arrowstyle="-", color=INK, lw=1.1))

    ax.set_xlim(-3, 82)
    ax.set_ylim(50, 140)
    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="Mismo punto de partida, distinto efecto de la edad")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11.5)
    for text in leg.get_texts():
        text.set_color(INK)
    eq_box(ax, "PAS = 60 + $\\beta_1 \\cdot$ Edad")
    save(fig, "03_mismo_intercepto_distinto_pendiente.pdf")


# ==========================================================================
# Diapositiva 4 -- covariable categorica (sexo): desplaza el intercepto
# ==========================================================================
def slide_04():
    beta1 = 0.5
    b0_m, b0_f = 100, 92
    x = np.linspace(20, 80, 200)

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    y_m = b0_m + beta1 * x
    y_f = b0_f + beta1 * x
    ax.plot(x, y_m, color=COLOR["blue"], linewidth=3.2, label="Hombres", zorder=3)
    ax.plot(x, y_f, color=COLOR["orange"], linewidth=3.2, label="Mujeres", zorder=3)

    x0 = 50
    ax.plot([x0, x0], [b0_f + beta1 * x0, b0_m + beta1 * x0], color=INK,
            linewidth=1.6, linestyle=(0, (4, 3)), zorder=2)
    ax.annotate(
        "Brecha constante en todo el rango de edad:\n"
        "$\\beta_{sexo}$ = 8 mmHg (Hombres vs. Mujeres)",
        xy=(x0, (b0_f + b0_m) / 2 + beta1 * x0), xytext=(15, 0),
        textcoords="offset points", va="center", fontsize=11.5, color=INK,
    )

    ax.set_xlim(15, 85)
    ax.set_ylim(105, 155)
    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="Covariable categórica (sexo): un desplazamiento vertical")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=12)
    for text in leg.get_texts():
        text.set_color(INK)
    eq_box(ax, "PAS = $\\beta_0$+$\\beta_1\\cdot$Edad+$\\beta_{sexo}\\cdot$Hombre", loc="lower left")
    save(fig, "04_covariable_categorica_sexo.pdf")


# ==========================================================================
# Diapositiva 5 -- dos variables continuas: plano + sus marginales (cortes)
# ==========================================================================
def slide_05():
    beta0, b_edad, b_imc = 100.0, 0.5, 1.2
    edad = np.linspace(20, 80, 25)
    imc = np.linspace(18, 40, 25)
    E, I = np.meshgrid(edad, imc)
    Y = beta0 + b_edad * E + b_imc * I

    edad_mean, imc_mean = edad.mean(), imc.mean()

    fig = plt.figure(figsize=(13.5, 7))
    gs = GridSpec(2, 3, width_ratios=[2.1, 2.1, 1.3], height_ratios=[1, 1],
                  wspace=0.45, hspace=0.55, figure=fig)

    ax3d = fig.add_subplot(gs[:, 0:2], projection="3d")
    ax3d.plot_surface(E, I, Y, color=COLOR["blue"], alpha=0.45, linewidth=0,
                       antialiased=True, shade=True, zorder=1)

    edad_line = np.linspace(20, 80, 60)
    y_edad_slice = beta0 + b_edad * edad_line + b_imc * imc_mean
    ax3d.plot(edad_line, np.full_like(edad_line, imc_mean), y_edad_slice,
              color=COLOR["orange"], linewidth=3.5, zorder=5)

    imc_line = np.linspace(18, 40, 60)
    y_imc_slice = beta0 + b_edad * edad_mean + b_imc * imc_line
    ax3d.plot(np.full_like(imc_line, edad_mean), imc_line, y_imc_slice,
              color=COLOR["aqua"], linewidth=3.5, zorder=5)

    style_3d(ax3d, "Edad (años)", "IMC (kg/m²)", "PAS (mmHg)",
             title="El plano de regresión\ncon dos variables continuas")

    ax_e = fig.add_subplot(gs[0, 2])
    ax_e.plot(edad_line, y_edad_slice, color=COLOR["orange"], linewidth=3)
    style_2d(ax_e, "Edad (años)", "PAS (mmHg)")
    ax_e.set_title("Corte a IMC = 29\n(promedio)", fontsize=11, color=COLOR["orange"],
                    fontweight="bold", loc="left")

    ax_i = fig.add_subplot(gs[1, 2])
    ax_i.plot(imc_line, y_imc_slice, color=COLOR["aqua"], linewidth=3)
    style_2d(ax_i, "IMC (kg/m²)", "PAS (mmHg)")
    ax_i.set_title("Corte a Edad = 50 años\n(promedio)", fontsize=11, color=COLOR["aqua"],
                    fontweight="bold", loc="left")

    fig.suptitle("Dos variables continuas: el plano y sus marginales (cortes)",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.01)
    fig.text(0.02, -0.02,
             "PAS = $\\beta_0$ + $\\beta_1\\cdot$Edad + $\\beta_2\\cdot$IMC   "
             "($\\beta_0$=100, $\\beta_1$=0.5, $\\beta_2$=1.2)",
             fontsize=11, color=INK_2)
    save(fig, "05_plano_dos_continuas_y_marginales.pdf")


# ==========================================================================
# Diapositiva 6 -- categorica + dos continuas: dos planos paralelos
# ==========================================================================
def slide_06():
    b_edad, b_imc = 0.5, 1.2
    b0_m, b0_f = 100.0, 92.0
    edad = np.linspace(20, 80, 20)
    imc = np.linspace(18, 40, 20)
    E, I = np.meshgrid(edad, imc)
    Y_m = b0_m + b_edad * E + b_imc * I
    Y_f = b0_f + b_edad * E + b_imc * I

    fig = plt.figure(figsize=SLIDE_SIZE)
    ax = fig.add_subplot(111, projection="3d")
    ax.plot_surface(E, I, Y_m, color=COLOR["blue"], alpha=0.45, linewidth=0,
                     antialiased=True, shade=True)
    ax.plot_surface(E, I, Y_f, color=COLOR["orange"], alpha=0.45, linewidth=0,
                     antialiased=True, shade=True)

    style_3d(ax, "Edad (años)", "IMC (kg/m²)", "PAS (mmHg)",
             title="Sexo + dos variables continuas: planos paralelos (sin interacción)")

    handles = [Patch(facecolor=COLOR["blue"], alpha=0.6, label="Hombres"),
               Patch(facecolor=COLOR["orange"], alpha=0.6, label="Mujeres")]
    leg = ax.legend(handles=handles, loc="upper left", frameon=False, fontsize=12,
                     bbox_to_anchor=(0.0, 0.95))
    for text in leg.get_texts():
        text.set_color(INK)

    fig.text(0.5, 0.06,
              "PAS = $\\beta_0$ + $\\beta_1\\cdot$Edad + $\\beta_2\\cdot$IMC + $\\beta_{sexo}\\cdot$Hombre",
              ha="center", fontsize=12, color=INK, fontweight="bold")
    fig.text(0.5, 0.02,
              "Los dos planos tienen la misma inclinación: el efecto de Edad y de IMC es igual "
              "en Hombres y Mujeres; solo cambia la altura (el intercepto).",
              ha="center", fontsize=10.5, color=INK_2)
    save(fig, "06_dos_planos_sexo_mas_dos_continuas.pdf")


# ==========================================================================
# Diapositiva 7 -- interaccion: las pendientes dejan de ser iguales
# ==========================================================================
def slide_07():
    b0_m, b1_m = 90.0, 0.7
    b0_f, b1_f = 105.0, 0.30
    x = np.linspace(20, 80, 200)

    fig = plt.figure(figsize=(13, 6.75))
    gs = GridSpec(1, 2, width_ratios=[1.55, 1], wspace=0.35, figure=fig)

    ax = fig.add_subplot(gs[0, 0])
    y_m = b0_m + b1_m * x
    y_f = b0_f + b1_f * x
    ax.plot(x, y_m, color=COLOR["blue"], linewidth=3.2, label=f"Hombres  ($\\beta_1$={b1_m})")
    ax.plot(x, y_f, color=COLOR["orange"], linewidth=3.2, label=f"Mujeres  ($\\beta_1$={b1_f})")

    x_cross = (b0_f - b0_m) / (b1_m - b1_f)
    y_cross = b0_m + b1_m * x_cross
    ax.plot([x_cross], [y_cross], "o", color=INK, markersize=8, zorder=5)
    ax.annotate("Las líneas se cruzan:\nel efecto de la edad\ndepende del sexo",
                xy=(x_cross, y_cross), xytext=(20, -55), textcoords="offset points",
                fontsize=11, color=INK,
                arrowprops=dict(arrowstyle="-", color=INK, lw=1.1))

    ax.set_xlim(15, 85)
    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="Interacción Edad × Sexo")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11.5)
    for text in leg.get_texts():
        text.set_color(INK)
    eq_box(ax, "PAS = $\\beta_0$+$\\beta_1$Edad+$\\beta_{s}$Sexo+$\\beta_{int}$(Edad$\\times$Sexo)",
           loc="lower right", fontsize=10.5)

    ax3d = fig.add_subplot(gs[0, 1], projection="3d")
    edad = np.linspace(20, 80, 15)
    imc = np.linspace(18, 40, 15)
    E, I = np.meshgrid(edad, imc)
    Y_m = b0_m + b1_m * E + 1.0 * I
    Y_f = b0_f + b1_f * E + 1.0 * I
    ax3d.plot_surface(E, I, Y_m, color=COLOR["blue"], alpha=0.45, linewidth=0, shade=True)
    ax3d.plot_surface(E, I, Y_f, color=COLOR["orange"], alpha=0.45, linewidth=0, shade=True)
    style_3d(ax3d, "Edad", "IMC", "PAS", title="En 3D: planos\nya no son paralelos",
             elev=18, azim=-50)

    fig.suptitle("PAS = $\\beta_0$ + $\\beta_1\\cdot$Edad + $\\beta_{sexo}\\cdot$Sexo + "
                  "$\\beta_{int}\\cdot$(Edad$\\times$Sexo)",
                  fontsize=13, color=INK_2, y=1.02)
    save(fig, "07_interaccion.pdf")


# ==========================================================================
# Diapositiva 8 -- efectos multinivel: la pendiente varia entre clinicas
# ==========================================================================
def slide_08():
    rng = np.random.default_rng(42)
    beta1 = 0.5
    b0_m, b0_f = 100.0, 92.0
    x = np.linspace(20, 80, 100)
    n_clinicas = 6

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)

    for grupo, (b0, base_color) in {
        "Hombres": (b0_m, COLOR["blue"]),
        "Mujeres": (b0_f, COLOR["orange"]),
    }.items():
        for _ in range(n_clinicas):
            b0_j = b0 + rng.normal(0, 5)
            b1_j = beta1 + rng.normal(0, 0.1)
            ax.plot(x, b0_j + b1_j * x, color=base_color, alpha=0.30,
                    linewidth=1.3, zorder=2)
        ax.plot(x, b0 + beta1 * x, color=base_color, linewidth=3.4,
                zorder=4, label=f"Promedio {grupo.lower()}")

    ax.set_xlim(15, 85)
    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="Efectos multinivel: la pendiente también varía entre clínicas")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=12)
    for text in leg.get_texts():
        text.set_color(INK)

    ax.text(0.02, 0.03,
            f"Cada línea delgada = una clínica distinta ({n_clinicas} por sexo, simuladas).\n"
            "La línea gruesa es el promedio poblacional para ese sexo.",
            transform=ax.transAxes, fontsize=10.5, color=INK_2)
    eq_box(ax, "PAS$_{ij}$ = ($\\beta_0$+$u_{0j}$) + ($\\beta_1$+$u_{1j}$)Edad", loc="lower right")
    save(fig, "08_multinivel_pendientes_por_clinica.pdf")


# ==========================================================================
# Diapositiva 9 -- de lo no lineal a lo lineal: Edad y Edad^2 como dos ejes
# ==========================================================================
def slide_09():
    edad_c = np.linspace(-30, 30, 300)          # Edad - 50
    riesgo = edad_c + edad_c**2

    edad_sq_mean = 300.0                         # E[Edad_c^2] para Edad_c ~ U(-30,30)

    fig = plt.figure(figsize=(14.5, 8))
    gs = GridSpec(3, 2, width_ratios=[2.3, 1], hspace=0.85, wspace=0.32, figure=fig)

    # --- Panel grande: el plano en el espacio (Edad_c, Edad_c^2) ---------
    ax3d = fig.add_subplot(gs[:, 0], projection="3d")
    x1_grid = np.linspace(-30, 30, 25)
    x2_grid = np.linspace(0, 900, 25)
    X1, X2 = np.meshgrid(x1_grid, x2_grid)
    Y_plane = X1 + X2
    ax3d.plot_surface(X1, X2, Y_plane, color=COLOR["aqua"], alpha=0.35,
                       linewidth=0, shade=True, zorder=1)

    # curva real de los datos (Edad_c, Edad_c^2, riesgo): vive EXACTAMENTE sobre el plano
    t = np.linspace(-30, 30, 150)
    ax3d.plot(t, t**2, t + t**2, color=COLOR["red"], linewidth=3.5, zorder=6,
              label="Datos reales: (Edad, Edad², riesgo)")

    # proyeccion de esa misma curva sobre el "piso" (z minimo): aqui se ve la parabola
    # de pie, sin escorzo -- es la razon geometrica de por que Edad^2 "endereza" la curva.
    z_floor = -60
    ax3d.plot(t, t**2, np.full_like(t, z_floor), color=COLOR["red"], linewidth=2,
              linestyle=(0, (2, 2)), alpha=0.65, zorder=2)
    ax3d.plot([t[0], t[0]], [t[0]**2, t[0]**2], [z_floor, t[0] + t[0]**2],
              color=COLOR["red"], linewidth=0.8, alpha=0.3, zorder=2)

    # los dos cortes (marginales) del plano, igual que en la diapositiva del plano continuo
    edad_line = np.linspace(-30, 30, 40)
    ax3d.plot(edad_line, np.full_like(edad_line, edad_sq_mean), edad_line + edad_sq_mean,
              color=COLOR["blue"], linewidth=3, zorder=5)
    sq_line = np.linspace(0, 900, 40)
    ax3d.plot(np.zeros_like(sq_line), sq_line, sq_line,
              color=COLOR["orange"], linewidth=3, zorder=5)

    ax3d.set_zlim(z_floor, 950)
    style_3d(ax3d, "Edad centrada", "Edad²", "Riesgo",
             title="En el espacio (Edad, Edad²) la relación es un plano",
             elev=26, azim=-52)
    ax3d.text2D(0.02, 0.02, "línea punteada = la parábola \"de pie\", proyectada en el piso",
                transform=ax3d.transAxes, fontsize=9, color=COLOR["red"])

    # --- Panel superior derecho: la curva real (no lineal en Edad) -------
    ax1 = fig.add_subplot(gs[0, 1])
    ax1.plot(edad_c, riesgo, color=COLOR["red"], linewidth=3)
    style_2d(ax1, "Edad centrada (años)", "Riesgo", title=None)
    ax1.set_title("La relación real: curva", fontsize=12.5, color=COLOR["red"],
                   fontweight="bold", loc="left")
    eq_box(ax1, "riesgo = Edad + Edad²", loc="upper left", fontsize=10.5)

    # --- Panel medio derecho: corte a Edad_c^2 fija -> lineal en Edad ----
    ax2 = fig.add_subplot(gs[1, 1])
    edad_line2 = np.linspace(-30, 30, 40)
    ax2.plot(edad_line2, edad_line2 + edad_sq_mean, color=COLOR["blue"], linewidth=3)
    style_2d(ax2, "Edad centrada (años)", "Riesgo", title=None)
    ax2.set_title("Corte a Edad² fija (=300, su promedio)", fontsize=11.5, color=COLOR["blue"],
                   fontweight="bold", loc="left")
    eq_box(ax2, "riesgo = Edad + 300", loc="upper left", fontsize=10.5)

    # --- Panel inferior derecho: corte a Edad_c fija -> lineal en Edad^2 -
    ax3 = fig.add_subplot(gs[2, 1])
    sq_line2 = np.linspace(0, 900, 40)
    ax3.plot(sq_line2, sq_line2, color=COLOR["orange"], linewidth=3)
    style_2d(ax3, "Edad² (años²)", "Riesgo", title=None)
    ax3.set_title("Corte a Edad fija (=0, su promedio)", fontsize=11.5, color=COLOR["orange"],
                   fontweight="bold", loc="left")
    eq_box(ax3, "riesgo = Edad²", loc="upper left", fontsize=10.5)

    fig.suptitle("De lo no lineal a lo lineal: agregar Edad² como una nueva variable",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.01)
    save(fig, "09_no_lineal_edad_cuadrado_plano.pdf")


# ==========================================================================
if __name__ == "__main__":
    print("Generando figuras (parte 1) en:", "figuras/")
    slide_01()
    slide_02()
    slide_03()
    slide_04()
    slide_05()
    slide_06()
    slide_07()
    slide_08()
    slide_09()
    print("Listo (parte 1).")
