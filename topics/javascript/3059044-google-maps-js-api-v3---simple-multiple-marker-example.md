
# Google Maps JS API v3 - Simple Multiple Marker Example

## Question
        
Fairly new to the Google Maps Api. I've got an array of data that I want to cycle through and plot on a map. Seems fairly simple, but all the multi-marker tutorials I have found are quite complex.

Let's use the data array from google's site for an example:

    var locations = [
      ['Bondi Beach', -33.890542, 151.274856, 4],
      ['Coogee Beach', -33.923036, 151.259052, 5],
      ['Cronulla Beach', -34.028249, 151.157507, 3],
      ['Manly Beach', -33.80010128657071, 151.28747820854187, 2],
      ['Maroubra Beach', -33.950198, 151.259302, 1]
    ];
    

I simply want to plot all of these points and have an infoWindow pop up when clicked to display the name.

## Answer
        
This is the simplest I could reduce it to:

    <!DOCTYPE html>
    <html> 
    <head> 
      <meta http-equiv="content-type" content="text/html; charset=UTF-8" /> 
      <title>Google Maps Multiple Markers</title> 
      <script src="http://maps.google.com/maps/api/js?sensor=false" 
              type="text/javascript"></script>
    </head> 
    <body>
      <div id="map" style="width: 500px; height: 400px;"></div>
    
      <script type="text/javascript">
        var locations = [
          ['Bondi Beach', -33.890542, 151.274856, 4],
          ['Coogee Beach', -33.923036, 151.259052, 5],
          ['Cronulla Beach', -34.028249, 151.157507, 3],
          ['Manly Beach', -33.80010128657071, 151.28747820854187, 2],
          ['Maroubra Beach', -33.950198, 151.259302, 1]
        ];
    
        var map = new google.maps.Map(document.getElementById('map'), {
          zoom: 10,
          center: new google.maps.LatLng(-33.92, 151.25),
          mapTypeId: google.maps.MapTypeId.ROADMAP
        });
    
        var infowindow = new google.maps.InfoWindow();
    
        var marker, i;
    
        for (i = 0; i < locations.length; i++) {  
          marker = new google.maps.Marker({
            position: new google.maps.LatLng(locations[i][1], locations[i][2]),
            map: map
          });
    
          google.maps.event.addListener(marker, 'click', (function(marker, i) {
            return function() {
              infowindow.setContent(locations[i][0]);
              infowindow.open(map, marker);
            }
          })(marker, i));
        }
      </script>
    </body>
    </html>
    

Screenshot:

![Google Maps Multiple Markers](https://i.imgur.com/mM82YXg.png)

There is some closure magic happening when passing the callback argument to the `addListener` method. This can be quite a tricky topic, if you are not familiar with how closures work. I would suggest checking out the following Mozilla article for a brief introduction, if it is the case:

*   [Mozilla Dev Center: Working with Closures](https://developer.mozilla.org/en/Core_JavaScript_1.5_Guide/Working_with_Closures)
