# info201-final-project
## Team: 40plus1

## Link: [https://ryantqiu.shinyapps.io/info201_final_project/](https://ryantqiu.shinyapps.io/info201_final_project/)

### Feature Artist by Genre Tab:

Here the user is presented with checkboxes to select genres and attributes. Once specific genres and attributes have been selected the data is taken and formatted into a radar chart which will display the levels of each attribute and categorize each genre by color.
This layout grants an efficient way to view many different attributes of many different genres. It allows a user to analyze the overall attributes of genres relative to other genres.  

### Create a Playlist Tab:
Here the user can select between two different subtabs. The first subtab Playlist Creation allows the user search for specific tracks within a specific genre. Once a user has selected a track the Music Analyzer present that individual track’s radar chart containing the levels for attributes that can be selected to the left of the radar chart. If the user likes the track they can push the “Add this song to your playlist” button to add the song into your playlist. All the songs in the user’s playlist are displayed in a sidebar panel with a radar chart displaying the average attribute data from all the songs in the playlist. Once the user is satisfied with their playlist they can then go to the Song Recommendation subtab and press the  “Press to see songs similar to the ones in your playlist” button to see the tracks from the data with the most similar attribute levels.
These features can be used to assist the user by introducing tracks that they are likely to enjoy.   

### Popularity Trend Tab:
Here the user can choose a genre and an attribute to plot. Once the genre and attribute have been selected, the plot will filter and select the corresponding tracks from the data and will plot the data according to the popularity. 
This can be used to analyze what attributes of specific genres result in a popular/successful track.

### About This Project: 
The design team “40 Plus 1” would like to introduce our newest app, Music Analyzer. Running as a potential extension of Spotify, Music Analyzer uses data from kaggle user tomigelo’s “Spotify Audio Features” which contains audio features from 130 thousand collected from the official Spotify Web API. Music Analyzer takes the information from the database and can display differences, similarities, trends, and even recommendations between whole genres and specific tracks.

Team 40 Plus 1:
Team 40 Plus 1 is a team formed through the INFO 201 course at the University of Washington. 
Team Members:  
Ryan Qiu
Christian Kim 
John Lee 

### Attribute definitions:
#### artist_name: Shows the name of the artist excluding featured artists 

#### track_name: Shows the name of the track, includes featured artists

#### acousticness: Is a confidence measure from 0.0 to 1.0 of whether the track is acoustic (not having electrical amplification).

#### danceability: Is a confidence measure from 0.0 to 1.0 that describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability/consistency, and beat strength.

#### duration_ms: Is the length of the track by milliseconds.

#### energy: Is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy.

#### instrumentalness: The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.

#### key: Is the estimated overall key of the track. 

#### liveness: Is the level of detection of an audience in the track (higher liveness indicates a higher likelihood of the track having been played live.  

#### loudness: Is the overall loudness of the track in decibels (dB).

#### mode: Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.

#### speechiness: Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.

#### tempo: The overall estimated tempo of a track in beats per minute (BPM). Tempo is the speed or pace of a given piece and derives directly from the average beat duration.

#### time_signature: An estimated overall time signature of a track.

#### valence: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive, while tracks with low valence sound more negative.

