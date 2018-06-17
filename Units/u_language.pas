unit u_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  COUNTRY_COUNT = 5;
  CountryRes : array[0..COUNTRY_COUNT-1] of string =
     ( 'Français', 'English', 'Italiano', 'Deutsche', 'Español' );

var

  FCurrentCountry: integer=0;
  StrRes : array[0..13,0..COUNTRY_COUNT-1] of string ;


implementation
var i: integer;
Initialization
i := 0 ;
StrRes[i,0] := 'Commencer';    // 0
StrRes[i,1] := 'Start';
StrRes[i,2] := 'Cominciare';
StrRes[i,3] := 'Spiel starten';
StrRes[i,4] := 'comenzar';
inc(i);

StrRes[i,0] := 'Quitter';  // 1
StrRes[i,1] := 'Exit';
StrRes[i,2] := 'Uscire';
StrRes[i,3] := 'Spiel beenden';
StrRes[i,4] := 'Salir';
inc(i);

StrRes[i,0] := 'Bienvenue '; // 2
StrRes[i,1] := 'Welcome ';
StrRes[i,2] := 'Benvenuto ';
StrRes[i,3] := 'Willkommen ';
StrRes[i,4] := 'Bienvenida ';
inc(i);

StrRes[i,0] := '▼ Nouveau joueur';  // 3     ▼=ALT31
StrRes[i,1] := '▼ New player';
StrRes[i,2] := '▼ Nuovo giocatore';
StrRes[i,3] := '▼ Neuer Spieler';
StrRes[i,4] := '▼ Nuevo jugador';
inc(i);

StrRes[i,0] := 'Clic sur ton nom'; // 4
StrRes[i,1] := 'Click your name';
StrRes[i,2] := 'Clicca sul tuo nome';
StrRes[i,3] := 'Klick auf deinen Namen';
StrRes[i,4] := 'Haga clic en su nombre';
inc(i);

StrRes[i,0] := 'Utilise la roulette de la souris pour faire défiler la liste'; // 5
StrRes[i,1] := 'Use the mouse wheel to scroll through the list';
StrRes[i,2] := 'Usa la rotellina del mouse per scorrere l''elenco';
StrRes[i,3] := 'Benutze das Mausrad um in der Liste zu scrollen.';
StrRes[i,4] := 'Usa la rueda del mouse para desplazarte por la lista';
inc(i);

StrRes[i,0] := 'Dans le jeu, presse ''Echap'' pour revenir au menu'; // 6
StrRes[i,1] := 'In game, press ''ESC'' to return to the main menu';
StrRes[i,2] := 'Nel gioco, premere "Esc" per tornare al menu';
StrRes[i,3] := 'Drücke im Spiel auf die Taste ''Escape'', um zum Menü zurückzukehren';
StrRes[i,4] := 'En el juego, presiona '' Esc '' para regresar al menú';
inc(i);

StrRes[i,0] := 'Entre ton nom:'; // 7
StrRes[i,1] := 'Enter your name:';
StrRes[i,2] := 'Tra il tuo nome:';
StrRes[i,3] := 'Gebe deinen Namen ein:';
StrRes[i,4] := 'Entre tu nombre:';
inc(i);

StrRes[i,0] := 'Aller au suivant'; // 8
StrRes[i,1] := 'Go to next';
StrRes[i,2] := 'Vai a quello successivo';
StrRes[i,3] := 'Weiter zum nächsten Teil';
StrRes[i,4] := 'Ir a la siguiente';
inc(i);

StrRes[i,0] := 'Jeu terminé'; // 9
StrRes[i,1] := 'Game finished';
StrRes[i,2] := 'Fine del gioco';
StrRes[i,3] := 'Ende des Spiels';
StrRes[i,4] := 'Fin del juego';
inc(i);

// 10
StrRes[i,0] := 'Le jeu consiste à retrouver des points dans le bon ordre, '+
               'afin de compléter un dessin.'+LineEnding+LineEnding+
               'Pour t''aider, une petite fée suit la souris à l''écran '+
               'et t''indique la direction du point suivant par un trait.'+LineEnding+LineEnding+
               'Plus tu es proche du point, plus la fée tourne vite !'+LineEnding+LineEnding+
               'Lorsque le nombre de points à retrouver est important, un bonus apparaîtra. '+
               'Clique dessus pour l''attraper, il t''aidera à remettre plusieurs points à leur place !';
StrRes[i,1] := 'The game consists of finding points in the right order,' +
               'to complete a drawing.' + LineEnding+LineEnding +
               'To help you, a little fairy follows the mouse on the screen ' +
               'and show you the direction of the next point by a line.' + LineEnding + LineEnding +
               'The closer you are to the point, the faster the fairy turns!' + LineEnding + LineEnding +
               'When the number of points to find is important, a bonus will appear. '+
               'Click on it to catch it, it will help you to put several points in their right place!';

StrRes[i,2] := 'Il gioco consiste nel trovare i punti nell''ordine giusto,'+
               'per completare un disegno.' + LineEnding + LineEnding +
               'Per aiutarti, una piccola fata segue il mouse sullo schermo ' +
               'e mostra la direzione del punto successivo in base a una linea.' + LineEnding + LineEnding +
               'Più ti avvicini al punto, più velocemente la fata gira !' + LineEnding + LineEnding +
               'Quando il numero di punti da trovare è importante, apparirà un bonus. '+
               'Clicca sopra per prenderlo, ti aiuterà a mettere diversi punti al loro posto!';

StrRes[i,3] := 'Ziel des Spiels ist es, Punkte in der richtigen Reihenfolge zu finden, um damit ein Bild zu vervollständigen.' + LineEnding + LineEnding +
               'Um dir zu helfen, folgt eine kleine Fee der Maus auf dem Bildschirm und zeigt dir die Richtung des nächsten Punktes mit einem Strich an.' + LineEnding + LineEnding +
               'Je näher du dem Punkt kommst, desto schneller dreht sich die Fee!' + LineEnding + LineEnding +
               'Wenn die Anzahl der zu findenden Punkte groß ist, erscheint ein Bonus. Klicke auf den Bonus um ihn einzufangen. '+
               'Er wird dir helfen mehrere Punkte an ihre Stelle zu setzen !';

StrRes[i,4] := 'El juego consiste en encontrar puntos en el orden correcto ' +
               'para completar un dibujo' + LineEnding + LineEnding +
               'Para ayudarte, un pequeño hada sigue al ratón en la pantalla ' +
               'y le mostrará la dirección del siguiente punto por una línea.' + LineEnding + LineEnding +
               'Cuanto más cerca estés del punto, más rápido girará el hada!' + LineEnding + LineEnding +
               'Cuando la cantidad de puntos para encontrar es importante, aparecerá una bonificación. '+
               'Haz clic para atraparlo, ¡te ayudará a poner varios puntos en su lugar !';
inc(i);

StrRes[i,0] := 'Instructions';  // 11
StrRes[i,1] := 'Instructions';
StrRes[i,2] := 'Istruzione';
StrRes[i,3] := 'Erklärungen';
StrRes[i,4] := 'Instrucciones';
inc(i);

StrRes[i,0] := '◄ Retour';  // 12
StrRes[i,1] := '◄ Back';
StrRes[i,2] := '◄ Indietro';
StrRes[i,3] := '◄ Zurück';
StrRes[i,4] := '◄ Espalda';
inc(i);

StrRes[i,0] := 'Nouveau joueur';  // 13
StrRes[i,1] := 'New player';
StrRes[i,2] := 'Nuovo giocatore';
StrRes[i,3] := 'Neuer Spieler';
StrRes[i,4] := 'Nuevo jugador';
inc(i);

end.

