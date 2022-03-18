# Edgebreaker in Elm

![image](https://user-images.githubusercontent.com/43552143/158927694-11376864-580b-4a18-9e4f-cf001b17535c.png)

An Elm application for running the Edgebreaker algorithm against ply files which encode triangular meshes.

## Using The Application

The application is best used by visiting it's web address (here: https://barnesjt.github.io/elm-ply/index.html).

The first pages gives 2 options to begin: 

1. **Load PLY from File**: This allows you to upload a ply file (note: the app is meant to only handle triangular meshes, while other inputs won't crash the application, it's output is not guaranteed to be correct).
2. **Select a model from the dropdown**: This is the safer option, as it will load one of the ply files from the repository. Do note that files with more faces will take longer to load.

After loading a model, you'll notice that the model can be rotated by holding a mouse button and moving it. The application will load on a mobile browser, but touch controls aren't enabled for rotating the model.

The model can be changed by clicking the button at the button right that says "LOAD NEW MODEL (RESET)".

On the left-hand side is:
- Field of View slider (This acts a bit like zooming).
- Basic model info is displayed: filename, number of faces and number of vertices.
- Toggle button for the render display, default is the CLERS view (each face displays one of five colors, which keys the traversal operation at this face).
  - The secondary render mode is single color (orange), with a reflective surface.  
- Edgebreaker Output:
  - CLERS frequency: This is the frequency of each operation, and is also a legend for the CLERS render.
  - Complete CLERS Output: A list of each operation in order.
  - Complete Delta Output: This encodes all the vertex data that is needed to reconstruct the original mesh (Reconstruction not implemented).

## Building the Application

This is not recommended for most users, as a working version of the application can be accessed from any modern browser. Nevertheless, it is relatively simple to build this yourself. You will just need to install Elm: https://guide.elm-lang.org/install/elm.html, clone the repository and either run ```elm reactor``` to run locally, or ```elm make src/Main.elm``` to build an HTML file. No additional setup or custom html is required for this application.

## Future Work
- Load in custom encoding data to display original geometry
