<!DOCTYPE HTML><html>
<head>
    <title>AptMaster SF</title>
    <script type="text/javascript" src="/static/jquery-1.4.4.js" > </script>
    <script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false" > </script>
    <script type="text/javascript"><![CDATA[
        /* here? */
        function initialize() {
            var latlng = new google.maps.LatLng(-34.0, 150.0);
            var myOptions = {
                zoom: 8,
                center: latlng,
                mapTypeId: google.maps.MapTypeId.ROADMAP
            };
            map = new google.maps.Map(document.getElementById("map_canvas"), myOptions);

            $.getJSON('/poi/json', {}, function(pois) {
                for ( i in pois ) {
                    var p = pois[i]
                    var m = new google.maps.Marker({
                        "clickable": True,
                        "title": p.address,
                        "visible": True,
                        "draggable": False,
                        "map": map,
                        "position": new google.maps.LatLng(p.lat, p.lng, False),
                    })
                }
            });
        }
    ]]></script>
</head>
<body onload="initialize()">
    <div id="main">
        <div id="map_canvas" style="width:500px; height:400px"> </div>
        <div id="table_area"> </div>
    </div>
</body>
</html>
