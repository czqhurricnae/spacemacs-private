function initTTML() {
  var audioElements = document.querySelectorAll( "audio" );
  var slide = Reveal.getCurrentSlide();
  for( var i = 0; i < audioElements.length; i++ ) {
      audioElements[i].addEventListener( "play", function ( event ) {
      //Track object generated by the first <track> child element
      var myTrack = event.target.textTracks[0];
      //Src is only available from the <track> DOM element, not the obj
      var ttmlUrl = event.target.getElementsByTagName("track")[0].src;
      // var renderDiv = document.getElementById( "sub-" + event.target.id );
      var renderDiv = document.getElementById( "render-div" );
      var url = new URL(ttmlUrl);

      if ( url.pathname != "null" ) {
          //The track is disabled by default, 'hidden' does not display VTT Text
          //but fires the events
          myTrack.mode = "hidden";

          var client = new XMLHttpRequest();

          client.open('GET', ttmlUrl);
          client.onreadystatechange = function () {
              initTrack(client.responseText);
          }
          client.send();
      }

      function initTrack(text) {
        var imscDoc = imsc.fromXML(text);
        var timeEvents = imscDoc.getMediaTimeEvents();
        //create cue per timed event and render isd
        for (var i = 0; i < timeEvents.length; i++) {
          //Edge/IE implement "Generic Cue", other browsers VTTCue
          var Cue = window.VTTCue || window.TextTrackCue;
          if (i < timeEvents.length - 1) {
            //We have to provide empty string as VTTText
            var myCue = new Cue(timeEvents[i], timeEvents[i + 1], "");
          } else {
          /*
          "End" time of last "imsc event" is end of video
          */
            var myCue = new Cue(timeEvents[i], event.target.duration, "");
          }
          myCue.onenter = function () {
            clearSubFromScreen();
            var myIsd = imsc.generateISD(imscDoc, this.startTime);
            imsc.renderHTML(myIsd, renderDiv);
          };
          myCue.onexit = function () {
            clearSubFromScreen();
          };
          var r = myTrack.addCue(myCue);
        }
      }

      function clearSubFromScreen() {
        var subtitleActive = renderDiv.getElementsByTagName("div")[0];
        if (subtitleActive) {
          renderDiv.removeChild(subtitleActive);
        }
      }
      });
    }
}

window.addEventListener("load", initTTML);
