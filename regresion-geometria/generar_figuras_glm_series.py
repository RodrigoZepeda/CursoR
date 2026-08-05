"""
Parte 2 de 2 -- Genera las figuras de residuales, distribuciones (Normal y
Poisson como el promedio de una recta), GLM (logistica binaria y
multinomial) y series de tiempo (AR/MA/ARIMA) para la presentacion
"Regresion, geometricamente".

Corre con:
    python3 generar_figuras_glm_series.py

Requiere que estilo_figuras.py este en la misma carpeta.

Historias de variables:
    - Pacientes (diapositivas 10-13, 17-19):
        y  = PAS (mmHg) o Hipertension (Normal / Elevada / Hipertension)
        x  = Edad (años)
    - Dengue simulado (diapositivas 14-16, 20-22): datos simulados con fines
      ilustrativos, inspirados en el capitulo de series de tiempo del curso.
        y  = Casos de dengue por semana (conteo)
        x  = Temperatura promedio semanal (°C)
        t  = semana
"""

from math import lgamma, log, exp, sqrt, pi

import numpy as np
from matplotlib.gridspec import GridSpec

import matplotlib.pyplot as plt

from estilo_figuras import (
    COLOR, INK, INK_2, MUTED, GRID, AXIS, SURFACE, SLIDE_SIZE,
    save, style_2d, style_3d, style_mini, eq_box,
)


def poisson_pmf(ks, lam):
    ks = np.asarray(ks)
    return np.array([exp(-lam + k * log(lam) - lgamma(k + 1)) if k >= 0 else 0.0
                      for k in ks])


def normal_pdf(x, mu, sigma):
    return np.exp(-(x - mu) ** 2 / (2 * sigma ** 2)) / (sigma * sqrt(2 * pi))


def acf(x, nlags):
    x = np.asarray(x, dtype=float) - np.mean(x)
    n = len(x)
    c0 = np.dot(x, x) / n
    out = np.empty(nlags + 1)
    for k in range(nlags + 1):
        out[k] = (np.dot(x[: n - k], x[k:]) / n) / c0
    return out


def plot_acf(ax, series, nlags, color, title):
    r = acf(series, nlags)
    lags = np.arange(nlags + 1)
    ax.bar(lags[1:], r[1:], color=color, width=0.55, zorder=3)
    band = 1.96 / sqrt(len(series))
    ax.axhline(band, color=INK_2, linewidth=1, linestyle=(0, (4, 3)), zorder=2)
    ax.axhline(-band, color=INK_2, linewidth=1, linestyle=(0, (4, 3)), zorder=2)
    ax.axhline(0, color=AXIS, linewidth=1)
    style_2d(ax, "rezago (semanas)", "autocorrelación")
    ax.set_title(title, fontsize=12.5, color=color, fontweight="bold", loc="left")
    ax.set_ylim(-1, 1)


# Parametros compartidos de la historia "pacientes" (Edad -> PAS), reutilizados
# en residuales (10-11) y en la diapositiva Normal (12-13).
BETA0, BETA1 = 100.0, 0.5
SD_HOMO = 6.0

# Parametros de la logistica multinomial: PAS categorizada en 3 niveles,
# referencia = Normal.
MULTI_B0_E, MULTI_B1_E = -1.6, 0.02   # Elevada vs. Normal
MULTI_B0_H, MULTI_B1_H = -4.2, 0.07   # Hipertensión vs. Normal


# ==========================================================================
# Diapositiva 10 -- residuales: la diferencia entre lo observado y lo predicho
# ==========================================================================
def slide_10():
    rng = np.random.default_rng(7)
    n = 40
    x = rng.uniform(20, 80, n)
    y = BETA0 + BETA1 * x + rng.normal(0, SD_HOMO, n)

    b1_hat, b0_hat = np.polyfit(x, y, 1)
    xs = np.linspace(18, 82, 100)
    yhat = b0_hat + b1_hat * xs

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.plot(xs, yhat, color=COLOR["blue"], linewidth=3, zorder=3,
            label="Línea ajustada (predicho, $\\hat{y}$)")
    ax.scatter(x, y, s=34, color=COLOR["blue"], alpha=0.35, zorder=2,
               label="Pacientes observados")

    order = np.argsort(x)
    highlight_idx = order[np.linspace(0, n - 1, 7).astype(int)]
    for i in highlight_idx:
        yhat_i = b0_hat + b1_hat * x[i]
        color = COLOR["red"] if y[i] > yhat_i else COLOR["aqua"]
        ax.plot([x[i], x[i]], [yhat_i, y[i]], color=color, linewidth=2, zorder=4)
        ax.plot([x[i]], [y[i]], "o", color=color, markersize=7, zorder=5)

    i_pos = highlight_idx[np.argmax(y[highlight_idx] - (b0_hat + b1_hat * x[highlight_idx]))]
    i_neg = highlight_idx[np.argmin(y[highlight_idx] - (b0_hat + b1_hat * x[highlight_idx]))]
    yhat_pos = b0_hat + b1_hat * x[i_pos]
    yhat_neg = b0_hat + b1_hat * x[i_neg]
    ax.annotate("residual > 0\n(PAS observada por encima\nde la predicha)",
                xy=(x[i_pos], (y[i_pos] + yhat_pos) / 2), xytext=(15, 15),
                textcoords="offset points", fontsize=10.5, color=COLOR["red"])
    ax.annotate("residual < 0\n(PAS observada por debajo\nde la predicha)",
                xy=(x[i_neg], (y[i_neg] + yhat_neg) / 2), xytext=(15, -35),
                textcoords="offset points", fontsize=10.5, color=COLOR["aqua"])

    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="El residual: la distancia vertical entre el dato y la línea")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11)
    for t in leg.get_texts():
        t.set_color(INK)
    eq_box(ax, "residual$_i$ = $y_i$ − $\\hat{y}_i$ = $y_i$ − ($\\hat\\beta_0$+$\\hat\\beta_1 x_i$)")
    save(fig, "10_residuales_como_diferencias.pdf")


# ==========================================================================
# Diapositiva 11 -- los residuales deberian verse normales; homo vs. hetero
# ==========================================================================
def slide_11():
    rng = np.random.default_rng(11)
    n = 220
    x = rng.uniform(20, 80, n)

    y_homo = BETA0 + BETA1 * x + rng.normal(0, SD_HOMO, n)
    res_homo = y_homo - (BETA0 + BETA1 * x)

    sd_hetero = 2 + 0.16 * (x - 20)
    y_hetero = BETA0 + BETA1 * x + rng.normal(0, 1, n) * sd_hetero
    res_hetero = y_hetero - (BETA0 + BETA1 * x)

    fig = plt.figure(figsize=(13.5, 8.2))
    gs = GridSpec(2, 4, width_ratios=[3, 1, 3, 1], height_ratios=[1, 1.25],
                  wspace=0.15, hspace=0.55, figure=fig)
    xs = np.linspace(18, 82, 50)
    yline = BETA0 + BETA1 * xs

    ax_h = fig.add_subplot(gs[0, 0:2])
    ax_h.scatter(x, y_homo, s=18, color=COLOR["blue"], alpha=0.45, zorder=2)
    ax_h.plot(xs, yline, color=INK, linewidth=2.5, zorder=3)
    style_2d(ax_h, "Edad (años)", "PAS (mmHg)", title="Homocedástico")

    ax_e = fig.add_subplot(gs[0, 2:4])
    ax_e.scatter(x, y_hetero, s=18, color=COLOR["orange"], alpha=0.45, zorder=2)
    ax_e.plot(xs, yline, color=INK, linewidth=2.5, zorder=3)
    style_2d(ax_e, "Edad (años)", "PAS (mmHg)", title="Heterocedástico")

    ylim = (-32, 32)

    ax_rh = fig.add_subplot(gs[1, 0])
    ax_rh.axhspan(-2 * SD_HOMO, 2 * SD_HOMO, color=COLOR["blue"], alpha=0.07, zorder=0)
    ax_rh.scatter(x, res_homo, s=16, color=COLOR["blue"], alpha=0.5, zorder=2)
    ax_rh.axhline(0, color=INK, linewidth=1.4)
    ax_rh.set_ylim(*ylim)
    style_2d(ax_rh, "Edad (años)", "residual (mmHg)")
    ax_rh.set_title("La nube tiene el mismo\nancho en todo el rango", fontsize=11,
                     color=COLOR["blue"], fontweight="bold", loc="left")

    ax_hh = fig.add_subplot(gs[1, 1], sharey=ax_rh)
    counts, bins = np.histogram(res_homo, bins=16, range=ylim)
    centers = (bins[:-1] + bins[1:]) / 2
    ax_hh.barh(centers, counts, height=(bins[1] - bins[0]) * 0.95,
               color=COLOR["blue"], alpha=0.6)
    rr = np.linspace(*ylim, 200)
    dens = normal_pdf(rr, 0, SD_HOMO)
    ax_hh.plot(dens * n * (bins[1] - bins[0]), rr, color=INK, linewidth=1.8)
    ax_hh.set_ylim(*ylim)
    ax_hh.axis("off")
    ax_hh.text(0.5, 1.02, "se ve\nnormal", transform=ax_hh.transAxes, fontsize=9.5,
               color=INK_2, ha="center", va="bottom")

    ax_re = fig.add_subplot(gs[1, 2], sharey=ax_rh)
    sd_line = 2 + 0.16 * (xs - 20)
    ax_re.fill_between(xs, -2 * sd_line, 2 * sd_line, color=COLOR["orange"], alpha=0.07, zorder=0)
    ax_re.scatter(x, res_hetero, s=16, color=COLOR["orange"], alpha=0.5, zorder=2)
    ax_re.axhline(0, color=INK, linewidth=1.4)
    ax_re.set_ylim(*ylim)
    style_2d(ax_re, "Edad (años)", "")
    plt.setp(ax_re.get_yticklabels(), visible=False)
    ax_re.set_title("La nube se abre como un\nembudo con la edad", fontsize=11,
                     color=COLOR["orange"], fontweight="bold", loc="left")

    ax_he = fig.add_subplot(gs[1, 3], sharey=ax_rh)
    counts2, bins2 = np.histogram(res_hetero, bins=16, range=ylim)
    centers2 = (bins2[:-1] + bins2[1:]) / 2
    ax_he.barh(centers2, counts2, height=(bins2[1] - bins2[0]) * 0.95,
               color=COLOR["orange"], alpha=0.6)
    ax_he.set_ylim(*ylim)
    ax_he.axis("off")
    ax_he.text(0.5, 1.02, "mezcla de\nanchos", transform=ax_he.transAxes, fontsize=9.5,
               color=INK_2, ha="center", va="bottom")

    fig.suptitle("Los residuales deberían verse normales... ¿con qué varianza?",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.02)
    fig.text(0.02, -0.015,
              "Homocedasticidad (izquierda): varianza constante — el supuesto usual de la regresión lineal. "
              "Heterocedasticidad (derecha): la varianza cambia con Edad — el supuesto se rompe.",
              fontsize=11, color=INK_2)
    save(fig, "11_residuales_normalidad_varianza.pdf")


# ==========================================================================
# Diapositiva 12 -- la Normal cambia de posicion, NO de forma (sigma fija)
# ==========================================================================
def slide_12():
    mus = np.arange(95, 171, 5)  # 16 valores: 95, 100, ..., 170
    sigma = SD_HOMO

    fig, axes = plt.subplots(4, 4, figsize=(14, 9.5))
    ymax = normal_pdf(np.array([0.0]), 0, sigma)[0] * 1.15
    for ax, mu in zip(axes.flat, mus):
        xr = np.linspace(mu - 4 * sigma, mu + 4 * sigma, 200)
        dens = normal_pdf(xr, mu, sigma)
        ax.fill_between(xr, dens, color=COLOR["blue"], alpha=0.55, zorder=2)
        ax.plot(xr, dens, color=COLOR["blue"], linewidth=1.3, zorder=3)
        ax.axvline(mu, color=COLOR["red"], linewidth=1.1, linestyle=(0, (3, 2)), zorder=4)
        style_mini(ax)
        ax.set_ylim(0, ymax)
        ax.set_title(f"μ = {mu}", fontsize=11, color=INK, fontweight="bold", pad=3)
        ax.set_yticklabels([])

    fig.suptitle("La Normal cambia de posición... pero no de forma (σ constante)",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.01)
    fig.text(0.02, -0.01,
              "Cada panel es un promedio distinto de PAS, con la misma desviación estándar (σ = 6 mmHg). "
              "A diferencia de la Poisson, aquí la forma no cambia: la varianza es un parámetro aparte de la media.",
              fontsize=11.5, color=INK_2)
    fig.tight_layout(rect=[0, 0.01, 1, 0.95])
    save(fig, "12_normal_grid_promedios.pdf")


# ==========================================================================
# Diapositiva 13 -- la linea tambien es el promedio de una distribucion
# (aqui, Normal): el ancho NO cambia con la covariable
# ==========================================================================
def slide_13():
    sigma = SD_HOMO
    x = np.linspace(20, 80, 200)
    yline = BETA0 + BETA1 * x
    anchors = [25, 38, 50, 62, 75]

    r = np.linspace(-25, 25, 150)
    dens = normal_pdf(r, 0, sigma)
    scale = 0.75 * (anchors[1] - anchors[0]) / dens.max()

    fig, ax = plt.subplots(figsize=(12.5, 7.2))
    ax.plot(x, yline, color=COLOR["blue"], linewidth=2.6, zorder=3,
            label="PAS media = $\\beta_0$+$\\beta_1\\cdot$Edad")

    for a in anchors:
        mean_a = BETA0 + BETA1 * a
        ys = mean_a + r
        xs_right = a + dens * scale
        ax.fill_betweenx(ys, a, xs_right, color=COLOR["orange"], alpha=0.55, zorder=2)
        ax.axvline(a, color=AXIS, linewidth=0.8, zorder=1)
        ax.plot([a], [mean_a], "o", color=COLOR["red"], markersize=7, zorder=4)

    style_2d(ax, "Edad (años)", "PAS (mmHg)",
             title="La \"línea\" también es el promedio de una distribución")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11)
    for t in leg.get_texts():
        t.set_color(INK)
    eq_box(ax, "PAS | Edad ~ N($\\beta_0$+$\\beta_1\\cdot$Edad, $\\sigma^2$)", loc="lower right", fontsize=12)
    fig.text(0.02, -0.02,
              "Cada campana naranja es la distribución Normal completa en ese punto: a diferencia de la Poisson, "
              "aquí el ancho NO cambia con la Edad (σ constante = homocedasticidad).",
              fontsize=11, color=INK_2)
    fig.tight_layout(rect=[0, 0.02, 1, 1])
    save(fig, "13_normal_linea_es_promedio.pdf")


# ==========================================================================
# Diapositiva 14 -- la distribucion Poisson y su promedio
# ==========================================================================
def slide_14():
    lam = 5
    k = np.arange(0, 21)
    pmf = poisson_pmf(k, lam)

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.bar(k, pmf, color=COLOR["blue"], width=0.75, zorder=3)
    ax.axvline(lam, color=COLOR["red"], linewidth=2.2, linestyle=(0, (4, 3)), zorder=4)
    ax.annotate(f"media = λ = {lam}", xy=(lam, pmf.max()), xytext=(35, 0),
                textcoords="offset points", va="center", fontsize=13, color=COLOR["red"],
                fontweight="bold", arrowprops=dict(arrowstyle="-", color=COLOR["red"]))

    style_2d(ax, "Casos de dengue por semana (k)", "Probabilidad, P(Y = k)",
             title="La distribución Poisson: conteos descritos por un solo parámetro")
    ax.text(0.02, 0.04,
            "Ejemplo: número de casos de dengue por semana en un municipio pequeño",
            transform=ax.transAxes, ha="left", va="bottom", fontsize=11, color=INK_2,
            style="italic")
    eq_box(ax, "$P(Y=k) = \\dfrac{e^{-\\lambda}\\lambda^{k}}{k!}$", loc="upper right", fontsize=15)
    save(fig, "14_poisson_distribucion_y_promedio.pdf")


# ==========================================================================
# Diapositiva 15 -- la Poisson cambia de forma Y de posicion segun su promedio
# ==========================================================================
def slide_15():
    lams = [1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 18, 21, 25, 30, 35, 40]

    fig, axes = plt.subplots(4, 4, figsize=(14, 9.5))
    for ax, lam in zip(axes.flat, lams):
        kmax = int(lam + 4 * sqrt(lam)) + 3
        k = np.arange(0, kmax + 1)
        pmf = poisson_pmf(k, lam)
        ax.bar(k, pmf, color=COLOR["blue"], width=0.85, zorder=3)
        ax.axvline(lam, color=COLOR["red"], linewidth=1.1, linestyle=(0, (3, 2)), zorder=4)
        style_mini(ax)
        ax.set_title(f"λ = {lam}", fontsize=11, color=INK, fontweight="bold", pad=3)
        ax.set_yticklabels([])

    fig.suptitle("La forma y la posición de la Poisson cambian con su promedio (λ)",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.01)
    fig.text(0.02, -0.01,
              "Cada panel es un promedio distinto de casos de dengue por semana. Fíjate cómo la "
              "distribución no solo se recorre a la derecha: también se ensancha (la varianza crece con λ). "
              "Compara con la Normal de la diapositiva anterior, que solo se recorría.",
              fontsize=11.5, color=INK_2)
    fig.tight_layout(rect=[0, 0.01, 1, 0.95])
    save(fig, "15_poisson_grid_promedios.pdf")


# ==========================================================================
# Diapositiva 16 -- la linea es el promedio de una distribucion que tambien
# cambia de ancho (Poisson a lo largo de una covariable)
# ==========================================================================
def slide_16():
    beta0, beta1 = -4.97, 0.283
    temp = np.linspace(18, 34, 200)
    lam_curve = np.exp(beta0 + beta1 * temp)

    anchors = [20, 23, 26, 29, 32]
    lam_anchors = [exp(beta0 + beta1 * a) for a in anchors]
    max_pmf = max(poisson_pmf(np.arange(0, int(la + 4 * sqrt(la)) + 3), la).max()
                  for la in lam_anchors)
    scale = 0.72 * (anchors[1] - anchors[0]) / max_pmf

    fig, ax = plt.subplots(figsize=(12.5, 7.2))
    ax.plot(temp, lam_curve, color=COLOR["blue"], linewidth=2.6, zorder=3,
            label="$\\lambda$(Temperatura) = casos esperados por semana")

    for a, la in zip(anchors, lam_anchors):
        kmax = int(la + 4 * sqrt(la)) + 3
        k = np.arange(0, kmax + 1)
        pmf = poisson_pmf(k, la)
        ax.barh(k, pmf * scale, left=a, height=0.85, color=COLOR["orange"],
                alpha=0.55, zorder=2, edgecolor="none")
        ax.axvline(a, color=AXIS, linewidth=0.8, zorder=1)
        ax.plot([a], [la], "o", color=COLOR["red"], markersize=7, zorder=4)

    ax.set_ylim(0, 75)
    style_2d(ax, "Temperatura promedio semanal (°C)", "Casos de dengue por semana",
             title="La \"línea\" es solo el promedio de una distribución")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=11)
    for t in leg.get_texts():
        t.set_color(INK)
    eq_box(ax, "log($\\lambda$) = $\\beta_0$+$\\beta_1\\cdot$Temp   $\\Rightarrow$   $\\lambda = e^{\\beta_0+\\beta_1\\cdot Temp}$",
           loc="lower right", fontsize=12)
    fig.text(0.02, -0.02,
              "Cada abanico naranja es la distribución Poisson completa en ese punto: no solo sube el promedio "
              "con la temperatura, también crece su dispersión (en Poisson, Varianza = Media).",
              fontsize=11, color=INK_2)
    fig.tight_layout(rect=[0, 0.02, 1, 1])
    save(fig, "16_poisson_distribucion_se_mueve_con_covariable.pdf")


# ==========================================================================
# Diapositiva 17 -- regresion logistica: logit, momio y probabilidad
# ==========================================================================
def slide_17():
    beta0, beta1 = -3.3, 0.06
    edad = np.linspace(20, 80, 300)
    logit = beta0 + beta1 * edad
    odds = np.exp(logit)
    prob = 1 / (1 + np.exp(-logit))
    edad_corte = -beta0 / beta1

    fig, axes = plt.subplots(1, 3, figsize=(15.5, 6.5))

    ax1, ax2, ax3 = axes
    ax1.plot(edad, logit, color=COLOR["blue"], linewidth=3)
    style_2d(ax1, "Edad (años)", "logit(p) = ln(momio)", title="Escala logit: lineal")
    eq_box(ax1, "logit(p) = $\\beta_0$+$\\beta_1\\cdot$Edad", loc="upper left", fontsize=10.5)

    ax2.plot(edad, odds, color=COLOR["orange"], linewidth=3)
    style_2d(ax2, "Edad (años)", "Momio = p/(1−p)", title="Escala de momios: exponencial")
    eq_box(ax2, "momio = $e^{\\beta_0+\\beta_1\\cdot Edad}$", loc="upper left", fontsize=10.5)

    ax3.plot(edad, prob, color=COLOR["aqua"], linewidth=3, zorder=3)
    ax3.axvspan(20, edad_corte, color=COLOR["aqua"], alpha=0.06, zorder=0)
    ax3.axvspan(edad_corte, 80, color=COLOR["red"], alpha=0.06, zorder=0)
    ax3.axhline(0.5, color=INK, linewidth=1.2, linestyle=(0, (4, 3)), zorder=2)
    ax3.axvline(edad_corte, color=INK, linewidth=1.2, linestyle=(0, (4, 3)), zorder=2)
    ax3.plot([edad_corte], [0.5], "o", color=INK, markersize=8, zorder=5)
    ax3.annotate(f"punto de corte\nEdad ≈ {edad_corte:.0f} años (p = 0.5)",
                 xy=(edad_corte, 0.5), xytext=(10, -55), textcoords="offset points",
                 fontsize=10.5, color=INK, arrowprops=dict(arrowstyle="-", color=INK))
    ax3.text(22, 0.93, "clasificado:\nsin hipertensión", fontsize=9.5, color=COLOR["aqua"],
             fontweight="bold")
    ax3.text(63, 0.93, "clasificado:\ncon hipertensión", fontsize=9.5, color=COLOR["red"],
             fontweight="bold")
    style_2d(ax3, "Edad (años)", "Probabilidad de hipertensión", title="Escala de probabilidad: sigmoide")
    eq_box(ax3, "p = $\\dfrac{1}{1+e^{-(\\beta_0+\\beta_1 Edad)}}$", loc="lower right", fontsize=11)

    fig.suptitle("Regresión logística: la misma línea, tres escalas distintas",
                  fontsize=17, fontweight="bold", color=INK, x=0.01, ha="left", y=1.03)
    fig.text(0.5, -0.02, "Desenlace: Hipertensión (PAS ≥ 140 mmHg, sí/no)  ·  Covariable: Edad",
             ha="center", fontsize=11.5, color=INK_2)
    fig.tight_layout(rect=[0, 0.02, 1, 0.94])
    save(fig, "17_logistica_logit_momio_probabilidad.pdf")


# ==========================================================================
# Diapositiva 18 -- logistica multinomial (parte 1): una linea por categoria
# ==========================================================================
def slide_18():
    edad = np.linspace(20, 80, 200)
    logit_E = MULTI_B0_E + MULTI_B1_E * edad
    logit_H = MULTI_B0_H + MULTI_B1_H * edad

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.axhline(0, color=INK, linewidth=1.6, linestyle=(0, (4, 3)), zorder=2)
    ax.plot(edad, logit_E, color=COLOR["yellow"], linewidth=3, zorder=3,
            label="Elevada vs. Normal")
    ax.plot(edad, logit_H, color=COLOR["red"], linewidth=3, zorder=3,
            label="Hipertensión vs. Normal")
    ax.annotate("Normal (referencia): logit = 0", xy=(25, 0), xytext=(0, 8),
                textcoords="offset points", ha="left", fontsize=10.5, color=INK_2,
                style="italic")

    style_2d(ax, "Edad (años)", "logit[P(categoría) / P(Normal)]",
             title="Logística multinomial: una línea por categoría (vs. referencia)")
    leg = ax.legend(loc="upper left", frameon=False, fontsize=12)
    for t in leg.get_texts():
        t.set_color(INK)
    eq_box(ax, "logit[P(k)/P(Normal)] = $\\beta_{0k}$+$\\beta_{1k}\\cdot$Edad", loc="lower right",
           fontsize=12)
    fig.text(0.02, -0.02,
              "Desenlace de 3 categorías: PAS Normal (referencia) / Elevada / Hipertensión, según Edad. "
              "Con K categorías se ajustan K−1 líneas: cada una compara una categoría contra la referencia.",
              fontsize=11, color=INK_2)
    fig.tight_layout(rect=[0, 0.02, 1, 1])
    save(fig, "18_multinomial_logits.pdf")


# ==========================================================================
# Diapositiva 19 -- logistica multinomial (parte 2): probabilidades (softmax)
# ==========================================================================
def slide_19():
    edad = np.linspace(20, 80, 300)
    logit_E = MULTI_B0_E + MULTI_B1_E * edad
    logit_H = MULTI_B0_H + MULTI_B1_H * edad
    num_E, num_H = np.exp(logit_E), np.exp(logit_H)
    denom = 1 + num_E + num_H
    p_N, p_E, p_H = 1 / denom, num_E / denom, num_H / denom

    fig, ax = plt.subplots(figsize=SLIDE_SIZE)
    ax.stackplot(edad, p_N, p_E, p_H,
                 labels=["Normal", "Elevada", "Hipertensión"],
                 colors=[COLOR["blue"], COLOR["yellow"], COLOR["red"]],
                 alpha=0.78, zorder=2)

    edad0 = 60
    i0 = int(np.argmin(np.abs(edad - edad0)))
    y0, y1 = p_N[i0], p_N[i0] + p_E[i0]
    ax.axvline(edad0, color=INK, linewidth=1.6, linestyle=(0, (4, 3)), zorder=4)
    for y_mid, txt in [((0 + y0) / 2, f"{p_N[i0]*100:.0f}%"),
                        ((y0 + y1) / 2, f"{p_E[i0]*100:.0f}%"),
                        ((y1 + 1) / 2, f"{p_H[i0]*100:.0f}%")]:
        ax.annotate(txt, xy=(edad0, y_mid), xytext=(12, 0), textcoords="offset points",
                    va="center", fontsize=11, color=INK, fontweight="bold",
                    bbox=dict(boxstyle="round,pad=0.25", facecolor="white",
                              edgecolor=AXIS, linewidth=0.8))

    ax.set_ylim(0, 1)
    style_2d(ax, "Edad (años)", "Probabilidad",
             title="Las probabilidades se reparten el 100% entre categorías")
    leg = ax.legend(loc="upper left", frameon=True, framealpha=0.9, edgecolor=AXIS, fontsize=11)
    eq_box(ax, "P(k) = $e^{logit_k}$ / $\\Sigma_j\\, e^{logit_j}$   (softmax)", loc="lower right",
           fontsize=12)
    fig.text(0.02, -0.02,
              f"A los {edad0} años, la composición esperada es la que marca la línea punteada: las tres "
              "probabilidades siempre suman 100%, en cualquier edad.",
              fontsize=11, color=INK_2)
    fig.tight_layout(rect=[0, 0.02, 1, 1])
    save(fig, "19_multinomial_probabilidades_softmax.pdf")


# ==========================================================================
# Series de tiempo -- simulacion compartida (AR(1) + MA(1) sobre ruido)
# ==========================================================================
def simulate_series(T=150, phi=0.5, theta=0.4, sigma=3.0, seed=123):
    rng = np.random.default_rng(seed)
    t = np.arange(1, T + 1)
    seasonal = 10 * np.sin(2 * np.pi * t / 52)
    eps = rng.normal(0, sigma, T + 1)
    u = np.zeros(T + 1)
    for i in range(1, T + 1):
        u[i] = phi * u[i - 1] + eps[i] + theta * eps[i - 1]
    u = u[1:]
    y = 25 + seasonal + u
    return t, y, u


# ==========================================================================
# Diapositiva 20 -- el componente AR: x_t vs x_{t-1} es una regresion lineal
# ==========================================================================
def slide_20():
    t, y, u = simulate_series()

    fig = plt.figure(figsize=(13.5, 8.2))
    gs = GridSpec(2, 2, height_ratios=[1, 1.25], hspace=0.55, wspace=0.3, figure=fig)

    ax_top = fig.add_subplot(gs[0, :])
    ax_top.plot(t, y, color=COLOR["blue"], linewidth=1.8)
    style_2d(ax_top, "semana", "Índice semanal\n(transformado)",
             title="Serie semanal simulada")
    ax_top.text(0.99, 0.06, "datos simulados con fines ilustrativos",
                transform=ax_top.transAxes, ha="right", fontsize=9.5, color=INK_2,
                style="italic")

    ax_bl = fig.add_subplot(gs[1, 0])
    u_t, u_lag = u[1:], u[:-1]
    slope, intercept = np.polyfit(u_lag, u_t, 1)
    xs = np.linspace(u_lag.min(), u_lag.max(), 20)
    ax_bl.scatter(u_lag, u_t, s=20, color=COLOR["orange"], alpha=0.55, zorder=2)
    ax_bl.plot(xs, intercept + slope * xs, color=INK, linewidth=2.6, zorder=3)
    style_2d(ax_bl, "$x_{t-1}$ (semana anterior, sin tendencia/estacionalidad)",
             "$x_t$ (semana actual)")
    ax_bl.set_title(f"Componente AR: hoy vs. ayer  ($\\hat\\phi$ = {slope:.2f})", fontsize=12.5,
                     color=COLOR["orange"], fontweight="bold", loc="left")
    eq_box(ax_bl, "$x_t = \\phi\\cdot x_{t-1} + \\varepsilon_t$", loc="lower right", fontsize=12)

    ax_br = fig.add_subplot(gs[1, 1])
    plot_acf(ax_br, u, nlags=15, color=COLOR["blue"], title="Autocorrelación de $x_t$")

    fig.suptitle("El componente AR: \"hoy\" depende linealmente de \"ayer\"",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.02)
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    save(fig, "20_series_tiempo_componente_ar.pdf")


# ==========================================================================
# Diapositiva 21 -- separar la serie: lo que explica el AR vs. lo que sobra
# ==========================================================================
def slide_21():
    t, y, u = simulate_series()
    u_t, u_lag = u[1:], u[:-1]
    slope, intercept = np.polyfit(u_lag, u_t, 1)
    ar_fit = intercept + slope * u_lag
    resid = u_t - ar_fit
    t_e = t[1:]

    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(12.5, 9.8), sharex=True)

    ax1.plot(t_e, u_t, color=COLOR["blue"], linewidth=1.6)
    style_2d(ax1, "", "serie completa")
    ax1.set_title("1. Serie completa (sin tendencia/estacionalidad)", fontsize=13,
                   color=COLOR["blue"], fontweight="bold", loc="left")

    ax2.plot(t_e, ar_fit, color=COLOR["orange"], linewidth=1.9)
    style_2d(ax2, "", "componente AR")
    ax2.set_title(f"2. − Lo que predice el AR  ($\\hat\\phi\\cdot x_{{t-1}}$, $\\hat\\phi$={slope:.2f})",
                   fontsize=13, color=COLOR["orange"], fontweight="bold", loc="left")

    ax3.plot(t_e, resid, color=COLOR["red"], linewidth=1.6)
    ax3.axhline(0, color=AXIS, linewidth=1)
    style_2d(ax3, "semana", "residual")
    ax3.set_title("3. = Lo que sobra tras restar el AR (esto le toca al MA)", fontsize=13,
                   color=COLOR["red"], fontweight="bold", loc="left")

    fig.suptitle("Separar la serie: lo que explica el AR vs. lo que le queda",
                  fontsize=17, fontweight="bold", color=INK, x=0.02, ha="left", y=1.015)
    fig.text(0.02, -0.01,
              "serie completa  =  componente AR (predicho con $x_{t-1}$)  +  residual (lo que le toca al MA)",
              fontsize=12.5, color=INK, fontweight="bold")
    fig.tight_layout(rect=[0, 0.02, 1, 0.94])
    save(fig, "21_series_tiempo_separacion_ar_residual.pdf")


# ==========================================================================
# Diapositiva 22 -- el componente MA y la sintesis ARIMA
# ==========================================================================
def slide_22():
    t, y, u = simulate_series()
    u_t, u_lag = u[1:], u[:-1]
    slope, intercept = np.polyfit(u_lag, u_t, 1)
    e = u_t - (intercept + slope * u_lag)
    t_e = t[1:]

    fig = plt.figure(figsize=(13.5, 8.2))
    gs = GridSpec(2, 2, height_ratios=[1, 1.25], hspace=0.55, wspace=0.3, figure=fig)

    ax_top = fig.add_subplot(gs[0, :])
    ax_top.plot(t_e, e, color=COLOR["red"], linewidth=1.6)
    ax_top.axhline(0, color=AXIS, linewidth=1)
    style_2d(ax_top, "semana", "residual tras el ajuste AR",
             title="Lo que le queda a la serie después de quitar el componente AR")

    ax_bl = fig.add_subplot(gs[1, 0])
    e_t, e_lag = e[1:], e[:-1]
    slope_e, intercept_e = np.polyfit(e_lag, e_t, 1)
    xs = np.linspace(e_lag.min(), e_lag.max(), 20)
    ax_bl.scatter(e_lag, e_t, s=20, color=COLOR["violet"], alpha=0.55, zorder=2)
    ax_bl.plot(xs, intercept_e + slope_e * xs, color=INK, linewidth=2.6, zorder=3)
    style_2d(ax_bl, "$e_{t-1}$ (residual, semana anterior)", "$e_t$ (residual, semana actual)")
    ax_bl.set_title(f"Componente MA: el residual de hoy vs. el de ayer  ($\\hat\\theta$ ≈ {slope_e:.2f})",
                     fontsize=12, color=COLOR["violet"], fontweight="bold", loc="left")
    eq_box(ax_bl, "$e_t = \\theta\\cdot e_{t-1} + \\text{ruido}$", loc="lower right", fontsize=12)

    ax_br = fig.add_subplot(gs[1, 1])
    plot_acf(ax_br, e, nlags=15, color=COLOR["red"], title="Autocorrelación del residual")

    fig.suptitle("El componente MA: todavía queda una dependencia lineal por explicar",
                  fontsize=16.5, fontweight="bold", color=INK, x=0.02, ha="left", y=1.02)
    fig.text(0.02, -0.015,
              "ARIMA(p, d, q):  d = diferenciar hasta quitar tendencia  ·  p = regresión sobre "
              "rezagos de la propia serie (AR)  ·  q = regresión sobre rezagos del residual (MA).\n"
              "En cada paso, seguimos haciendo lo mismo del inicio de esta presentación: ajustar una línea recta.",
              fontsize=11.5, color=INK_2)
    fig.tight_layout(rect=[0, 0.03, 1, 0.93])
    save(fig, "22_series_tiempo_componente_ma_y_sintesis_arima.pdf")


# ==========================================================================
if __name__ == "__main__":
    print("Generando figuras (parte 2) en:", "figuras/")
    slide_10()
    slide_11()
    slide_12()
    slide_13()
    slide_14()
    slide_15()
    slide_16()
    slide_17()
    slide_18()
    slide_19()
    slide_20()
    slide_21()
    slide_22()
    print("Listo (parte 2).")
