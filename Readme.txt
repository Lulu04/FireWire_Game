FIREWIRE  -  created by Lulu  for Lazarus Game contest 2018


To be compiled, this game need package BGRABitmap and LazOpenGLContext installed in Lazarus IDE.

In game, use ESC key to return to main menu.

List of directories:


- Data contains sounds, graphics, levels and some other stuff for the game.

- Units  contains pascal units for the game.

- Level Editor folder contains units for tools to create new level for the game
  Save the new created level in folder 'Data\Drawings\'



List of directories in 'Units\'

- OALSoundManager  contains units for audio. It use OpenAL library and can play wav files 8bits or 16 bits, mono or stereo.

- OGLCScene  contains units for OpenGL scene. It use TOpenGLControl.
I start to write the framework 4/5 years ago slowly, according for my needs. It use OpenGL through TOpenGLControl. I got inspired by ZENGL written by Andrey Kemka, so this is not modern OpenGL programming. At this point, I use "glBegin" / glEnd" structure to draw graphics.


lulu
contact@lulutech.fr