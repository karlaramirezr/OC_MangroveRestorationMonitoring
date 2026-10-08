//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                  Supervised Classification HNTS
//                      Osa Conservation - 2025
//                        Karla Ramírez-Ruiz
//  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Here the link on GEE: https://code.earthengine.google.com/?accept_repo=users/karlaramirez/HNTS_multitemporal
// Get the first planetscope image filtered by the ROI

var image2023 = ee.Image('projects/mangroverestoration/assets/HNTS/2023_Planet');
var image2024 = ee.Image('projects/mangroverestoration/assets/HNTS/2024_Planet');
var image2025 = ee.Image('projects/mangroverestoration/assets/HNTS/2025_Planet');

print(image2023)
print(image2024)
print(image2025)

var variables = ['b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8'];

// Center the map on that image.
Map.centerObject(image2023, 13);

// Planetscope image true color visualization
var vis = { min: 0, max: 1500, bands: [ 'b6', 'b4', 'b2' ] };

// Show the image
Map.addLayer(image2023, vis, 'Planet_2023');
Map.addLayer(image2024, vis, 'Planet_2024');
Map.addLayer(image2025, vis, 'Planet_2025');

// Sample
var sample = ground.merge(mangrove).merge(Fern).merge(water).randomColumn();

// Split into training and test
var training = sample.filter(ee.Filter.lte('random', 0.8));
var test = sample.filter(ee.Filter.gt('random', 0.8));

// Extract value from image
var trainingSample = image2023.sampleRegions({
  collection: training,
  scale: 5,
  properties: ['class']
});
var testSample = image2023.sampleRegions({
  collection: test,
  scale: 5,
  properties: ['class']
});
print('Training sample', trainingSample.size(), 'Test sample', testSample.size());

// Random forest model
var model = ee.Classifier.smileRandomForest(50).train(trainingSample, 'class', variables);

// Check accuracy
var testClassify = testSample.classify(model, 'predicted_class').errorMatrix('class', 'predicted_class');
print('Confusion matrix', testClassify, 'Overall accuracy', testClassify.accuracy());


var classVis = {
  min: 1,
  max: 4,
  palette: ['800000', '00FF00', 'FFFF00', '0000FF']
};

// Show the classificaiton result 2023
var LandCover2023 = image2023.classify(model, 'LandCover2023')
  .set('LandCover_class_values', [1, 2, 3, 4], 'LandCover_class_palette', ['800000', '00FF00', 'FFFF00', '0000FF']);
Map.addLayer(LandCover2023, classVis, 'LandCover 2023');

// Clasificación 2024
var LandCover2024 = image2024.classify(model, 'LandCover2024')
  .set('LandCover_class_values', [1, 2, 3, 4], 
       'LandCover_class_palette', ['800000', '00FF00', 'FFFF00', '0000FF']);
Map.addLayer(LandCover2024, classVis, 'LandCover 2024');

// Clasificación 2025
var LandCover2025 = image2025.classify(model, 'LandCover2025')
  .set('LandCover_class_values', [1, 2, 3, 4], 
       'LandCover_class_palette', ['800000', '00FF00', 'FFFF00', '0000FF']);
Map.addLayer(LandCover2025, classVis, 'LandCover 2025');
