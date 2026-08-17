//=========================================================================
// Mangrove Cover Change 2017-2026 Humedal Nacional Térraba-Sierpe (CR)
// LAND COVER CHANGE - Sentinel-2 Harmonized - 2019 vs 2026
//=========================================================================

//-----------------------------------------------------
// INPUTS: studyArea and training polygons with class

// class = 0 Mangrove
// class = 1 Fern
// class = 3 Water
// class = 4 Artificial
// class = 5 Bare soil
// class = 6 Mixed or other vegetation

// Cloud Mask

function maskS2(image){

  var qa = image.select('QA60');

  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image
      .updateMask(mask)
      .divide(10000)
      .copyProperties(image, image.propertyNames());
}

//Image Collection

function getComposite(start,end){

  var collection = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
     .filterBounds(studyArea)
     // .filterBounds(HNTS_geom)
      .filterDate(start,end)
      .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',20))
      .map(maskS2);

  var composite = collection.median().clip(studyArea);

  return composite;

}

// Spectral Indices

function addIndices(image){

  var NDVI = image.normalizedDifference(['B8','B4'])
      .rename('NDVI');

  var NDMI = image.normalizedDifference(['B8','B11'])
      .rename('NDMI');

  var MNDWI = image.normalizedDifference(['B3','B11'])
      .rename('MNDWI');

  var NDBI = image.normalizedDifference(['B11','B8'])
      .rename('NDBI');

  var SAVI = image.expression(
      '1.5*((NIR-RED)/(NIR+RED+0.5))',{
        NIR:image.select('B8'),
        RED:image.select('B4')
      }).rename('SAVI');
      
  // Mangrove Vegetation Index (MVI)
  var MVI = image.expression( '(NIR - GREEN) / (SWIR - GREEN)',
  {
    NIR: image.select('B8'),
    GREEN: image.select('B3'),
    SWIR: image.select('B11')
  }
).rename('MVI');

  return image
      .addBands(NDVI)
      .addBands(NDMI)
      .addBands(MNDWI)
      .addBands(NDBI)
      .addBands(SAVI)
      .addBands(MVI);
}

//Composites

var img2019 = addIndices(
    getComposite('2019-01-01','2019-04-15'));
print(img2019);

var img2026 = addIndices(
    getComposite('2026-01-01','2026-04-15'));
print(img2026);
    
// Bands

var bands = [ 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B11',
'B12', 'NDVI', 'NDMI', 'MNDWI', 'NDBI', 'SAVI', 'MVI'];

// Visualization 

// True color: RGB
var trueColor = {bands: ['B4', 'B3', 'B2'], min: 0.02, max: 0.30, gamma: 1.2};
var mviVis = {min: 0,   max: 8,  palette: ['f7fcf5', 'c7e9c0', '74c476','238b45','00441b']};

Map.centerObject(studyArea, 12);

//Map.addLayer(img2019, trueColor, '2019 - True Color', true );
//Map.addLayer(img2019.select('MVI'), mviVis, '2019 - MVI'); //,  false);
//Map.addLayer(img2026, trueColor,  '2026 - True Color',  true );

//Export.image.toDrive({
//  image: img2019.select(bands).toFloat(),
//  description: 'Sentinel2_Composite_2019',
//  fileNamePrefix: 'Sentinel2_Composite_2018',
//  region: studyArea,
//  scale: 10,
//  maxPixels: 1e13
//});

// TRAINING

var training = img2019.select(bands).sampleRegions({
  collection: trainingPolygons,
  properties:['class'],
  scale:10
});

// Random Split

training = training.randomColumn();
var train = training.filter('random < 0.7');
var test = training.filter('random >= 0.7');

// Random Forest Classifier

var classifier = ee.Classifier.smileRandomForest({
  numberOfTrees:300,
  variablesPerSplit:4,
  minLeafPopulation:2,
  bagFraction:0.7,
  seed:1
}).train({
  features:train,
  classProperty:'class',
  inputProperties:bands
});

// Accuracy check

var validation = test.classify(classifier);
var matrix = validation.errorMatrix('class','classification');
//print('Confusion Matrix',matrix);
//print('Overall Accuracy',matrix.accuracy());
//print('Kappa',matrix.kappa());

// Land Cover Classification

var lc2019 = img2019.select(bands).classify(classifier);

var lc2026 = img2026.select(bands).classify(classifier);

// Calculate 

// Area by Class

function areaByClass(image){
  var area = ee.Image.pixelArea()
      .addBands(image);
  var table = area.reduceRegion({
      reducer:ee.Reducer.sum().group({
          groupField:1,
          groupName:'class'
      }),
      geometry:studyArea,
      scale:10,
      maxPixels:1e13
  });

  return table;
}

//print('Area 2019',areaByClass(lc2019));

//print('Area 2026',areaByClass(lc2026));


// Visualization Part II

var palette = [

'006400',   // Mangrove
'ffff00',   // Fern
'0000b3',   // Water
'ff0000',   // Artificial
'd2b48c' ,   // Bare soil
'646400'    // Other Vegetation
];

Map.addLayer(lc2019,{min:0,max:5, palette:palette},'Land Cover 2019');

Map.addLayer(lc2026,{min:0, max:5, palette:palette},'Land Cover 2026');

// Land Cover Change Map

//var change = lc2019.multiply(10).add(lc2026);
//Map.addLayer(change,{},'Change');

// Specific changes : MANGROVE / FERN TRANSITION MAP

// 0 = Other / irrelevant
// 1 = Stable mangrove
// 2 = Mangrove → Fern
// 3 = Fern → Mangrove
// 4 = Stable fern

var mfChange = ee.Image(0)

  // Stable mangrove
  .where(
    lc2019.eq(0).and(lc2026.eq(0)),
    1
  )
  // Mangrove → Fern
  .where(
    lc2019.eq(0).and(lc2026.eq(1)),
    2
  )
  // Fern → Mangrove
  .where(
    lc2019.eq(1).and(lc2026.eq(0)),
    3
  )
  // Stable fern
  .where(
    lc2019.eq(1).and(lc2026.eq(1)),
    4
  );

//Map.addLayer(mfChange, {min: 0, max: 4, palette: [
//      'FFFFFF',  // 0 = Other - white
//      '006400',  // 1 = Stable mangrove - dark green
//      'FFFF00',  // 2 = Mangrove → Fern - yellow
//      '00A000',  // 3 = Fern → Mangrove - medium green
//      'FFA500'   // 4 = Stable fern - orange
//    ]}, 'Mangrove - Fern Change', true );
    
var mfTransitions = mfChange.updateMask(
  mfChange.eq(2)
    .or(mfChange.eq(3))
);

Map.addLayer(mfTransitions, {min: 2, max: 3,
    palette: ['FFFF00',  // 2 = Mangrove → Fern
      '00A000'   // 3 = Fern → Mangrove
    ] },  'Mangrove ↔ Fern Transitions',  true);
