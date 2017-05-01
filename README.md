Supersonic-machine
==================

MPF machine description for Super Sonic pinball
Follow my progress here : https://thelegomoviepinball.wordpress.com/

![Supersonic playfield](/supersonic-monitor.PNG?raw=true "Playfield")


Rules
-----
I'm using a MAME Supersonic table to understand how rules are implemented...


### Base mode

#### Bumpers
Bumpers score 10
Lit bumpers score 100

##### Bumpers alternance
* on center fix target "2"
* on any top lane row
* on any drop target
* on any fixed target
* looks like any 300 score hit

#### Slingshots
Score 10

#### Bonus saucer
2x is lit
score 3000
bonus +1000
bonus is reset on new ball

#### Star roll over


#### Top lane
##### Top lane row 2 :
On hit:
* open the left rescue gate
* light the 1K spinner bonus

##### Roll over
* +3k bonus

#### Feed lane
It's the same sw both, and than the upper lane rebound.
Score 300

#### Drop target
3000 is lit at start.
When all targets down, 
* extraball is lit, score +3000
* extraball is win, special is lit, score +3000, lit "same player shoot again"
* score +3000, nothing more ?


### Missing rules

* how to light top "lanes score 3000" ?
* how to light "special" (left or right outlanes)
* what does "special" award ?


Card Rules
----------
### Card M-1508-75-E
* MAKING 1-2-3-4-5:
** 1ST TIME LITES CENTER STAR R.O. BUTTONS TO ADVANCE BONUS.
** 2ND TIME LITES JET STREAM R.O. BUTT,ONS TO ADVANCE BONUS.
** 3RD TIME LITES 1-5 LANES & TARGETS FOR 3,000 POINTS
AND OUTLANES FOR SPECIAL.
** 4TH TIME AND EACH ADDITIONAL TIME SCORES SPECIAL AND LITES CENTER STAR
AND JET STREAM 1,000 POINT LITES.
BALL THRU OUTLANE WHEN LIT FOR SPECIAL SCORES 1 REPLAY.
* SPINNER LITES WHEN BALL RETURN GATE IS OPEN.
* MAXIMUM — 1 EXTRA BALL PER BALL IN PLAY.
* TILT PENALTY — BALL IN PLAY.

### Card M-1508-75-F
* MAKING 1-2-3-4-5:
1ST TIME LITES CENTER STAR R.O. BUTTONS TO ADVANCE BONUS.
2ND TIME LITES JET STREAM R.O. BUTTONS TO ADVANCE BONUS.
3RD TIME LITES 1-5 LANES & TARGETS FOR 3,000 POINTS AND OUTLANES
FOR SPECIAL.
4TH TIME AND EACH ADDITIONAL TIME SCORES EXTRA BALL OR 25,000 POINTS AND
LITES CENTER STAR AND JET STREAM 1,000 POINT LITES.
BALL THRU OUTLANE WHEN LIT FOR SPECIAL SCORES EXTRA BALL OR 25,000 POINTS.
* SPINNER LITES WHEN BALL RETURN GATE IS OPEN.
* MAXIMUM — 1 EXTRA BALL PER BALL IN PLAY.
* TILT PENALTY — BALL IN PLAY.

### Card M-1508-75-G
* MAKING 1-2-3-4-5:
1ST TIME LITES CENTER STAR R.O. BUTTONS TO ADVANCE BONUS.
2ND TIME LITES JET STREAM R.O. BUTTONS TO ADVANCE BONUS.
3RD TIME LITES 1-5 LANES & TARGETS FOR 3,000 POINTS AND OUTLANES
FOR SPECIAL.
4TH TIME AND EACH ADDITIONAL TIME SCORES 25,000 POINTS.
BALL THRU OUTLANE WHEN LIT FOR SPECIAL SCORES 25,000 POINTS.
* ALL TARGETS DOWN WHEN LIT FOR EXTRA BALL OR SPECIAL SCORES 25,000 POINTS.
* SPINNER LITES WHEN BALL RETURN GATE IS OPEN.
* TILT PENALTY — BALL IN PLAY. 
