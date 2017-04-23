Supersonic-machine
==================

MPF machine description for Super Sonic pinball

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
it's the same sw both, and than the upper lane rebound
score 300

#### Drop target
3000 is lit at start
When all targets down, 
* extraball is lit, score +3000
* extraball is win, special is lit, score +3000, lit "same player shoot again"
* score +3000, nothing more ?


### Missing rules

* how to light top "lanes score 3000" ?
* how to light "special" (left or right outlanes)
* what does "special" award ?
