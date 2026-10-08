          ** 1.  Análisis de imágenes de dron: Cobertura del dosel de manglar**

La cobertura del dosel se refiere al porcentaje del suelo cubierto por la proyección vertical de las copas de las plantas.

Insumos:

Fotos de dron (RGB), georreferenciadas y ortorrectificadas, tomadas con dron de las parcelas de monitoreo de manglar (es decir, parcelas de 10 x 10 m dentro de los sitios de restauración) para los años 2023, 2024 y 2025. Todas las imágenes se llevaron a la misma resolución (~3.5 cm/píxel) para mejorar la comparabilidad.

Análisis:

Calculamos el Índice Normalizado de Exceso de Verde (Normalized Excess Green Index - NExG), usado para resaltar la vegetación verde. Usando un umbral de NExG < 0.1, clasificamos las imágenes en dos categorías: Vegetación (1) y No vegetación (0).

Con las imágenes de Vegetación/No vegetación para cada año (2023, 2024, 2025), pudimos calcular la cobertura del dosel para cada parcela de monitoreo que contaba con imágenes de dron en cada año.

Limitaciones:

Los índices espectrales probados para separar vegetación de suelo no pudieron diferenciar entre manglar y negraforra, probablemente porque ambos corresponden a vegetación verde de humedal y sus firmas espectrales son muy similares.

Por lo tanto, se corrigieron los valores de cobertura y se llevaron a 0 para las parcelas que evidentemente estaban dominadas por negraforra (Acrostichum aureum).

Este análisis calcula la cobertura del dosel. Por lo tanto, en las parcelas donde conviven manglar y negraforra, se calcula la cobertura del dosel sin discriminar entre especies. En consecuencia, en parcelas mixtas, donde coexisten manglar y negraforra, podría haber una sobreestimación de la cobertura del dosel de manglar, ya que la negraforra también contribuye a dicha cobertura.

Resultados:

Capas anuales (2023, 2024, 2025) del Índice Normalizado de Exceso de Verde (NExG) y hoja de cálculo con la cobertura del dosel de manglar por parcela de monitoreo y por año.

            **2.  Análisis de imágenes satelitales PlanetScope: Cobertura de la tierra en el HNTS**

La cobertura de la tierra se refiere a la cobertura biofísica que se observa sobre la superficie terrestre (por ejemplo, bosque, cuerpos de agua, cultivos, superficies artificiales, etc.).

Insumos:

Imágenes de Reflectancia de Superficie (SR), corregidas atmosféricamente, de PlanetScope (es decir, imágenes ortorrectificadas y radiométricamente corregidas que eliminan los efectos atmosféricos para proporcionar datos consistentes en diferentes momentos y ubicaciones), descargadas de https://www.planet.com/explorer/.

Se descargó una escena por año (2020-2025) que cubriera la totalidad del Humedal Nacional Térraba-Sierpe. Las imágenes se restringieron a la época seca (1 de enero al 30 de abril) de cada año para disminuir la prevalencia de nubes. Estas imágenes tienen un tamaño de píxel de aproximadamente 3 m/píxel y cuentan con 8 bandas.

Análisis:

Las imágenes de Planet para cada año se añadieron a Google Earth Engine para su análisis.

Se crearon polígonos para realizar una clasificación supervisada utilizando cuatro clases: Suelo desnudo, Manglar, Negraforra y Agua. El 80% de estos datos se usaron para entrenar un modelo de Random Forest para la clasificación y el 20% para evaluar la precisión del modelo.

Las áreas de entrenamiento se definieron a partir de la fotointerpretación de imágenes de dron (años 2023, 2024, 2025) e imágenes históricas de super alta resolución provistas por Google, además del conocimiento de la zona (por ejemplo, si un polígono fue restaurado en 2023, posiblemente era negraforra en años anteriores).

Para todos los años, se consideró que una evaluación aceptable del modelo debía presentar una precisión general (overall accuracy) mayor al 99%.

Aun así, se observa una pequeña confusión entre manglar y negraforra: el algoritmo clasifica algunos píxeles de manglar como negraforra, y viceversa. La precisión en la clasificación de suelo desnudo y agua fue del 100%. Esto puede deberse a la similitud de las firmas espectrales entre manglar y negraforra (ambas son vegetación verde de humedal).

Se intentó agregar otros índices espectrales que han sido utilizados para detectar manglares con imágenes Planet (como NDVI, NDWI, CMRI) y se realizó un pequeño ensayo usando la textura de la banda NIR (Gray Level Co-occurrence Matrix - GLCM), pero no se logró mejorar la clasificación entre manglar y negraforra.

Limitaciones:

- Inicialmente, se hizo un entrenamiento exhaustivo del modelo en el año 2023 y se utilizó ese entrenamiento para clasificar las imágenes de los demás años. Sin embargo, al evaluar el modelo con datos del año analizado, la precisión era ligeramente menor (~96%). Finalmente, se decidió tener puntos de entrenamiento y evaluación para cada año. No obstante, se sugiere que, en caso de una eventual publicación, se reevalúe cuál opción es más robusta estadísticamente.

- Aunque hay confianza en los puntos seleccionados gracias al conocimiento de la zona y a las imágenes de dron, podría existir cierta influencia de los puntos de entrenamiento escogidos sobre la evaluación del modelo y su precisión general. Sin embargo, se espera que esta influencia sea baja.

Resultados:

Capas anuales de cobertura de la tierra desde la disponibilidad de las imágenes (2020) hasta el presente (2025) e imágenes anuales de 8 bandas.

Hoja de cálculo con el área de suelo desnudo, manglar, negraforra y agua para cada año por cada sitio de restauración.

Scripts en Google Earth Engine de la clasificación de cada año, además de pruebas con otros índices espectrales, texturas y entrenamiento de clasificación en un año aplicada a otros años.

Datos clave:

La cobertura de manglar en los sitios de restauración en el HNTS aumentó de 14.65 ha en 2020 a 90.14 ha en 2025.

La cobertura de negraforra disminuyó de 181.96 ha en 2020 a 106.83 ha en 2025.

