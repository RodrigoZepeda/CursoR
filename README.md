# CursoR

Notas del curso de `R` del **Instituto Nacional de Salud Pública** (INSP), 2026.

**El libro se lee aquí: <https://rodrigozepeda.github.io/CursoR/>**

## Estructura

Este repositorio es un [libro de Quarto](https://quarto.org/docs/books/). Contiene
**sólo la documentación** (los archivos `.qmd`): las bases de datos no están aquí.

```
_quarto.yml        Configuración del libro y el orden de los capítulos
index.qmd          Portada
*.qmd              Un archivo por capítulo
references.bib     Bibliografía
_freeze/           Resultados ya calculados de los bloques de código
.github/workflows/ Publicación automática en GitHub Pages
```

## Los datos

Las bases pesan casi 1 GB, así que **viven en Dropbox, no en el repositorio**:

+ <https://www.dropbox.com/sh/yd73542zaptsjti/AACkyqBKuhJiR9mLEAVtXz2Wa?dl=0>

Para trabajar localmente descomprímelas en una carpeta `datasets/` en la raíz del
proyecto. Está en `.gitignore`, así que no se van a subir por accidente.

## Cómo se publica

Cada `push` a `master` dispara el workflow de GitHub Actions que construye el libro
y lo publica en GitHub Pages. El workflow **no instala `R` ni descarga las bases**:
usa los resultados congelados en `_freeze/`.

> [!IMPORTANT]
> **Si cambias el código de un capítulo**, corre `quarto render` en tu computadora
> y commitea el `_freeze/` actualizado. Si sólo cambias texto, no necesitas hacer
> nada. Cuando un capítulo cambió pero su `_freeze/` no, el workflow falla a
> propósito en lugar de publicar resultados viejos en silencio.

Para forzar que un capítulo se vuelva a calcular desde cero:

```bash
rm -rf _freeze/nombre-del-capitulo && quarto render
```

### Nota sobre el idioma del sistema

`R` necesita un *locale* UTF-8 para compilar el libro. Si te sale un error de
`unable to translate ... to native encoding`, corre:

```bash
export LANG=es_ES.UTF-8 LC_ALL=es_ES.UTF-8 && quarto render
```

## Contenido

0. Presentación
1. Preparación e instalación de `R` y `RStudio`
2. Introducción a `R`
3. Ciclos y condicionales
4. Gráficas con `ggplot2` (parte 1)
5. Gráficas con `ggplot2` (parte 2)
6. Análisis exploratorio de datos
7. Ejemplo de análisis exploratorio
8. Limpieza de bases y estadística descriptiva
9. Análisis de encuestas (ENSANUT)
10. Modelos: qué son y cómo se construyen
11. Regresiones (parte 1)
12. Regresiones (parte 2)
13. Cadenas de Markov
14. Ejemplo de reporte

Material externo de consulta:
[Tutorial de Quarto](https://pablolopez2733.github.io/Quarto-Tutorial/) (por Pablo
López Landeros).

## Contacto

> **Rodrigo Zepeda-Tello**
>
> _Github:_ [@RodrigoZepeda](https://github.com/RodrigoZepeda/)
>
> _Correo:_ [rzepeda17@gmail.com](mailto:rzepeda17@gmail.com)

> **Noé Osorio**
>
> _Correo:_ [ecostat.nog@gmail.com](mailto:ecostat.nog@gmail.com)
