import os
from qgis.core import (
    QgsRasterLayer,
    QgsProject
)
from qgis.analysis import (
    QgsRasterCalculator,
    QgsRasterCalculatorEntry
)

# --------------------------------------------------
# USER INPUT
# --------------------------------------------------
input_folder = r"D:/Landsat_Kanha_Moniter_2010_2011/USGS/Landsat_4_5/Mask/NDVI"
output_folder = r"D:/Landsat_Kanha_Moniter_2010_2011/USGS/Landsat_4_5/Mask/NDVI_Corrected"

a = -0.001
b = 1.021

os.makedirs(output_folder, exist_ok=True)

# --------------------------------------------------
# PROCESS ALL RASTERS
# --------------------------------------------------
for file in os.listdir(input_folder):

    if not file.lower().endswith(".tif"):
        continue

    input_path = os.path.join(input_folder, file)
    output_path = os.path.join(
        output_folder,
        file.replace(".tif", "_ETM.tif")
    )

    print(f"Processing: {file}")

    layer = QgsRasterLayer(input_path, file)
    if not layer.isValid():
        print(f"❌ Failed to load {file}")
        continue

    entry = QgsRasterCalculatorEntry()
    entry.ref = 'ndvi@1'
    entry.raster = layer
    entry.bandNumber = 1

    expression = f"{a} + {b} * ndvi@1"

    calc = QgsRasterCalculator(
        expression,
        output_path,
        'GTiff',
        layer.extent(),
        layer.width(),
        layer.height(),
        [entry]
    )

    result = calc.processCalculation()

    if result == 0:
        print(f"✅ Saved: {output_path}")
        QgsProject.instance().addMapLayer(
            QgsRasterLayer(output_path, os.path.basename(output_path))
        )
    else:
        print(f"❌ Error processing {file}")

print("🎯 NDVI TM → ETM+ batch correction completed!")