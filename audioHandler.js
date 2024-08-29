// audioHandler.js
function handleAudioResponse(request) {
    var blob = request.response; // Access the response as a Blob
    if (blob && blob.type === 'audio/mpeg') { // Check if the Blob is of the correct MIME type
        var url = window.URL.createObjectURL(blob); // Create a URL for the Blob

        // Get the TWebMultimediaPlayer instance from Pascal
        pas.Unit1.Form1.WebMultimediaPlayer1.URL = url; // Set the Blob URL to the multimedia player
        pas.Unit1.Form1.WebMultimediaPlayer1.Play(); // Play the audio directly

        // Set up the cleanup function to revoke the Blob URL after playback ends
        pas.Unit1.Form1.WebMultimediaPlayer1.OnEnded = function() {
            window.URL.revokeObjectURL(url); // Revoke the Blob URL to free up memory
        };
    } else {
        console.error('Incorrect file type received: ' + (blob ? blob.type : 'null')); // Log error if MIME type is incorrect
    }
}
