' *********************************************************************
' **                                                                 **
' **                        FUTURE PINBALL                           **
' **                                                                 **
' **                   Generic Table Script v1.0                     **
' **                                                                 **
' **     (C) 2007 Chris Leathley - BSP Software Design Solutions     **
' **                                                                 **
' ** This script should provide a simple frame work you can use in   **
' ** your own tables as a starting point.                            **
' **                                                                 **
' *********************************************************************

Option Explicit				' Force explicit variable declaration


' Define any Constants
Const constMaxPlayers 		= 4 		' Maximum number of players per game (between 1 and 4)
												' Set this to 0 if you don't want this feature
Const constMaxMultiplier	= 6		' Defines the maximum bonus multiplier level
Const constMaxCredits		= 25
Const Test						= False

' Define Global Variables
'
Dim PlayersPlayingGame		' number of players playing the current game
Dim CurrentPlayer				' current player (1-4) playing the game
Dim BonusPoints(4)			' Bonus Points for the current player
Dim BonusMultiplier(4)		' Bonus Multiplier for the current player
Dim BallsRemaining(4)		' Balls remaining to play (inclusive) for each player
Dim ExtraBallsAwards(4)		' number of EB's out-standing (for each player)
Dim FlagFreispiel1(4)
Dim FlagFreispiel2(4)
Dim Bb_Lane(5)					' 5 Lane lights
Dim Bb_LeftLane(5)			' 5 LeftLane lights
Dim Bb_Star(5)					' 5 Star lights
Dim Bb_Target(5)				' 5 Target lights
Dim Bb_Player(4)
Dim Bb_PlayerA(4)
Dim CountLaneLights(4)		' counts done lanes for each player
Dim CountLaneComplete(4)	' counts completion of all lanes for each player
Dim Bb_Bonus(10)				' 10 Bonus lights´
Dim FlagSpecial(4)

' Define Game Control Variables
Dim LastSwitchHit				' Id of last switch hit
Dim BallsOnPlayfield			' number of balls on playfield (multiball exclusive)
Dim PlungerPercentage		' to control plunger pull back
Dim SeeHUD						' to toggle the HUD-Display

' Define Game Flags
Dim bFreePlay					' Either in Free Play or Handling Credits
Dim bOnTheFirstBall			' First Ball (player one). Used for Adding New Players
Dim bBallInPlungerLane		' is there a ball in the plunger lane
Dim bMultiBallMode			' multiball mode active ?
Dim bEnteringAHighScore		' player is entering their name into the high score table
Dim FlagShowHighScore		' to toggle between Score and HighScore
Dim FlagCoinIn					' to enable credits
Dim FlagInitialising			' to block advance credits while initialising current coin
Dim FlagGameStart
Dim FlagScore
Dim FlagMenue
Dim FlagExtraBall
Dim FlagCoinLock
Dim FlagStartGame
Dim FlagLockStart

' Dim Variables
Dim HighScore					' contains the current HighScore
Dim ScoreCurrent5000
Dim ScoreNext5000
Dim ScoreCurrent1000
Dim ScoreNext1000
Dim ScoreCurrent100
Dim ScoreNext100
Dim ScoreCurrent10
Dim ScoreNext10
Dim Score3000
Dim ScoreCurrentKicker
Dim ScoreNextKicker
Dim Lane(4,5)
Dim Star(4,5)
Dim LeftLane(4,5)
Dim Lane3000(4)
Dim JetStream(4)
Dim Special(4)
Dim Bonus
Dim BonusStep
Dim BonusTemp
Dim BonusMultiplierTemp
Dim MatchPlay
Dim MatchNumber
Dim Replays
Dim FlagNameEntry
Dim NameEntry

Dim d




' *********************************************************************
' **                                                                 **
' **               Future Pinball Defined Script Events              **
' **                                                                 **
' *********************************************************************


' The Method Is Called Immediately the Game Engine is Ready to
' Start Processing the Script.
'
Sub FuturePinball_BeginPlay()
	' seed the randomiser (rnd(1) function)
	Randomize
	PlayMusic 1, "Initialze"
	FlagLockStart = True
	StartLockTimer.Enabled = True

	' initialise the upper lane rollover lights
	Set Bb_Lane(1)= Bb_Lane1: Set Bb_Lane(2)= Bb_Lane2: Set Bb_Lane(3)= Bb_Lane3
	Set Bb_Lane(4) = Bb_Lane4: Set Bb_Lane(5)= Bb_Lane5

	' initialise the left lane rollover lights
	Set Bb_LeftLane(1) = Bb_LeftLane1: Set Bb_LeftLane(2) = Bb_LeftLane2: Set Bb_LeftLane(3) = Bb_LeftLane3
	Set Bb_LeftLane(4) = Bb_LeftLane4: Set Bb_LeftLane(5) = Bb_LeftLane5

	' initialise the star rollover lights
	Set Bb_Star(1) = Bb_Star1: Set Bb_Star(2) = Bb_Star2: Set Bb_Star(3) = Bb_Star3
	Set Bb_Star(4) = Bb_Star4: Set Bb_Star(5) = Bb_Star5

	' initialise the target lights
	Set Bb_Target(1) = Bb_Target1: Set Bb_Target(2) = Bb_Target2: Set Bb_Target(3) = Bb_Target3
	Set Bb_Target(4) = Bb_Target4: Set Bb_Target(5) = Bb_Target5

	' initialise the bonus lights
	Set Bb_Bonus(1) = Bb_Bonus1000: Set Bb_Bonus(2) = Bb_Bonus2000: Set Bb_Bonus(3) = Bb_Bonus3000
	Set Bb_Bonus(4) = Bb_Bonus4000: Set Bb_Bonus(5) = Bb_Bonus5000: Set Bb_Bonus(6) = Bb_Bonus6000
	Set Bb_Bonus(7) = Bb_Bonus7000: Set Bb_Bonus(8) = Bb_Bonus8000: Set Bb_Bonus(9) = Bb_Bonus9000: Set Bb_Bonus(10) = Bb_Bonus10000

	' initialise the backglas player lights
	Set Bb_Player(1) = Bb_Player1: Set Bb_Player(2) = Bb_Player2: Set Bb_Player(3) = Bb_Player3: Set Bb_Player(4) = Bb_Player4
	Set Bb_PlayerA(1) = Bb_Player1a: Set Bb_PlayerA(2) = Bb_Player2a: Set Bb_PlayerA(3) = Bb_Player3a: Set Bb_PlayerA(4) = Bb_Player4a

	' initalise the player display to the last known player scores
	Display1.SetValue(nvScore1): If nvScore1 = 0 Then Display1.Text = "00"
	Display2.SetValue(nvScore2): If nvScore2 = 0 Then Display2.Text = "00"
	Display3.SetValue(nvScore3): If nvScore3 = 0 Then Display3.Text = "00"
	Display4.SetValue(nvScore4): If nvScore4 = 0 Then Display4.Text = "00"
	HudDisplay.SetValue(nvScore1): If nvScore1 = 0 Then HudDisplay.Text = "00"

	' initialise Balls in play display
	If nvR2 <> 0 then
		DisplayBallInPlay.SetValue(nvR2)
	Else
		DisplayBallInPlay.SetValue(30)
	End If

	' initialise Credits display
	If nvCredits  < 10 Then
		DisplayCredits.Text = "0" & nvCredits
	Else
		DisplayCredits.SetValue(nvCredits)
	End If

	If nvCredits > 0 Then Bb_ApronCredits.State = BulbOn

	' initialise balls per game
	If nvS1 = "" Then nvS1 = "3"
	Menue.Text = nvS1
	MenueOVL.FadeOut: Menue.FadeOut: NameEntryOVL.FadeOut
	If nvS1 = "5" Then nvBallsPerGame = 5: Replay3Ball.Render = False: Replay5Ball.Render = True
	If nvS1 = "3" Then nvBallsPerGame = 3: Replay3Ball.Render = True: Replay5Ball.Render = False

	If nvS1 = "3" Then
		If nvR12 < 340000 Then nvR12 = 340000
	Else
		If nvR12 < 560000 Then nvR12 = 560000
	End If

	HighScore = nvR12

	'initialise NameEntry
	If nvS2 = "" Then nvS2 = "J"
	If nvS2 = "J" Then NameEntry = True
	If nvS2 = "N" Then NameEntry = False

	'initialise HudDisplay
	If nvS3 = "" Then nvS3 = "True"

	If nvS3 = "True" Then
		HudDisplay.FadeIn()
		HudOVL.FadeIn()
	Else
		HudDisplay.FadeOut()
		HudOVL.FadeOut()
	End If

	' kill the last switch hit (this this variable is very usefull for game control)
	set LastSwitchHit = DummyTrigger

	' initialse any other flags
	bOnTheFirstBall = FALSE
	bEnteringAHighScore	= FALSE
	BallsOnPlayfield = 0
	SeeHUD = 1

	' initialise Bulbs on Backbox
	Bb_GameOver1.State = BulbOn
	Bb_GameOver2.State = BulbOn
	Bb_Match.State = BulbOn

	' Set the Machine to Attract Mode
	SetAllLightsForAttractMode()
	FlagShowHighScore = True
	BreakHighScoreTimer.Enabled = True
End Sub


Sub StartLockTimer_Expired()
	StartLockTimer.Enabled = False
	FlagLockStart = False
End Sub

' This Method is Called when the user has exited the Game Player. It should
' save any RAM variables that need to be persistant.
'
Sub FuturePinball_EndPlay()
End Sub


' The User Has Pressed A Key on the Keyboard..
'
Sub FuturePinball_KeyPressed(ByVal KeyCode)

	'AddDebugText "KeyCode = " & KeyCode
	'AddDebugText "Titled = " & fpTilted
	'
	' Process any keys which are valid at all times
	'

	' Test keys
	If Test = True Then
		If (KeyCode = 22) Then UpperRollover_Hit() ' U
		If (KeyCode = 79) Then Lane1_Hit() ' 1
		If (KeyCode = 80) Then Lane2_Hit() ' 2
		If (KeyCode = 81) Then Lane3_Hit() ' 3
		If (KeyCode = 75) Then Lane4_Hit() ' 4
		If (KeyCode = 76) Then Lane5_Hit() ' 5
		If (KeyCode = 37) Then Kicker_Hit() ' K
		If (KeyCode = 48) Then Bumper1_Hit() ' B
		If (KeyCode = 49) Then Bumber2_Hit() ' N
		If (KeyCode = 32) Then ' D
			d = d + 1
			DropTarget.PopDown(d): DropTarget_Hit()
			If d = 5 Then d = 0
		End If
		If (KeyCode = 19) Then StarRollover1_Hit() ' R
		If (KeyCode = 25) Then PlungerLaneTrigger_Hit() ' P
		IF (KeyCode = 24) Then LeftOutlane_Hit() ' O
		If (KeyCode = 45) Then Drain_Hit() ' X
	End If

	' The Player has Inserted a Coin
	If (KeyCode = GetKeyCode(InsertCoinKey)) Then
		If FlagCoinLock = True Then Exit Sub
		PlaySound "Coin"
		FlagCoinLock = True
		CoinTimer.Enabled = True
	End If

	' Show/Hide HudSegment
	If KeyCode = GetKeyCode(ToggleHudKey) Then												' toggle HUD
		If nvS3 = "False" Then
			HudDisplay.FadeIn()
			HudOVL.FadeIn()
			nvS3 = "True"
		Else
			HudDisplay.FadeOut()
			HudOVL.FadeOut()
			nvS3 = "False"
		End If
	End If

	' The Player is Pulling back the Plunger
	If (KeyCode = GetKeyCode(PlungerKey)) Then PlungerTimer.Enabled = True

	'
	' Process The Next set of keys depeding of wether there is a game in progress or not
	'

	' Is A Game in Progress?
	If (fpGameInPlay = TRUE) Then
		' and not Tilted ?
		If (fpTilted = FALSE) Then

			' If the Left Flipper Key Has Been Press, Activate The Left Flipper(s)
			If (KeyCode = GetKeyCode(LeftFlipperKey)) Then
				LeftFlipper.SolenoidOn
				PlaySound "FlipperUp"
			End If

			' If the Right Flipper Key Has Been Press, Activate The Right Flipper(s)
			If (KeyCode = GetKeyCode(RightFlipperKey)) Then
				RightFlipper.SolenoidOn
				PlaySound "FlipperUp"
			End If

			' Another player starting?
			If (KeyCode = GetKeyCode(StartGameKey)) Then
				If ((PlayersPlayingGame < constMaxPlayers) And (bOnTheFirstBall = TRUE)) Then
					If FlagCoinLock = True And PlayersPlayingGame < 4 Then
						FlagStartGame = True
						Exit Sub
					End If

					If (nvCredits > 0) then
						PlayersPlayingGame = PlayersPlayingGame + 1

						Select Case PlayersPlayingGame
							Case 2
								Display2.Text = "00"
								StopMusic 1: PlayMusic 2, "StartGame"
								Bb_Player2a.State = BulbOn
							Case 3
								Display3.Text = "00"
								StopMusic 1: PlayMusic 2, "StartGame"
								Bb_Player3a.State = BulbOn
							Case 4
								Display4.Text = "00"
								StopMusic 1: PlayMusic 2, "StartGame"
								Bb_Player4a.State = BulbOn
						End Select

						nvTotalGamesPlayed = nvTotalGamesPlayed + 1
						nvCredits = nvCredits - 1
						If nvCredits = 0 Then Bb_ApronCredits.State = BulbOff

						If nvCredits  < 10 Then
							DisplayCredits.Text = "0" & nvCredits
						Else
							DisplayCredits.SetValue(nvCredits)
						End If
					End If
				End If
			End If
		End If ' If (fpTilted)
	Else
      If (KeyCode = GetKeyCode(StartGameKey)) Then
			If FlagLockStart = True Then Exit Sub
			If FlagCoinLock = True Then
				FlagStartGame = True
				Exit Sub
			End If

			If (nvCredits > 0) Then
				If (BallsOnPlayfield = 0) Then
					StopMusic 1: PlayMusic 1, "StartGame"
					Bb_Player1a.State = BulbOn
					nvCredits = nvCredits - 1
					If nvCredits = 0 Then Bb_ApronCredits.State = BulbOff

					If nvCredits  < 10 Then
						DisplayCredits.Text = "0" & nvCredits
					Else
						DisplayCredits.SetValue(nvCredits)
					End If

					ResetForNewGame()
				End If
			End If
		End If

		' Show or hide Menue
		If FlagMenue = True Then
			If (KeyCode = GetKeyCode(RightFlipperKey)) Then
				If nvS1 = "5" Then
					Menue.Text = "3"
					nvBallsPerGame = 3
					nvS1 = "3"
					Replay3Ball.Render = True
					Replay5Ball.Render = False
					nvR12 = 340000
					HighScore = nvR12
				Else
					Menue.Text = "5"
					nvBallsPerGame = 5
					nvS1 = "5"
					Replay5Ball.Render = True
					Replay3Ball.Render = False
					nvR12 = 560000
					HighScore = nvR12
				End If
			End If
			If (KeyCode = GetKeyCode(Special2Key)) Then
				MenueOVL.FadeOut
				Menue.FadeOut
				FlagMenue = False
			End If
		Else
			If (KeyCode = GetKeyCode(Special2Key)) Then
				If FlagNameEntry = True Then Exit Sub
				MenueOVL.FadeIn
				Menue.FadeIn
				If nvS1 = "5" Then Menue.Text = "5"
				If nvS1 = "3" Then Menue.Text = "3"
				FlagMenue = True
			End If
		End If

		If FlagNameEntry = True Then
			If (KeyCode = GetKeyCode(RightFlipperKey)) Then
				If nvS2 = "J" Then
					Menue.Text = "N"
					nvS2 = "N"
					NameEntry = False
				Else
					Menue.Text = "J"
					nvS2 = "J"
					NameEntry = True
				End If
			End If
			If (KeyCode = GetKeyCode(Special1Key)) Then
				NameEntryOVL.FadeOut
				Menue.FadeOut
				FlagNameEntry = False
			End If
		Else
			If (KeyCode = GetKeyCode(Special1Key)) Then
				If FlagMenue = True Then Exit Sub
				NameEntryOVL.FadeIn
				Menue.FadeIn
				If nvS2 = "J" Then Menue.Text = "J"
				If nvS2 = "N" Then Menue.Text = "N"
				FlagNameEntry = True
			End If
		End If
	End If ' If (fpGameInPlay)
End Sub


' The User Has Released A Key on the Keyboard..
'
Sub FuturePinball_KeyReleased(ByVal KeyCode)

	'
	' Process any keys which are valid at all times
	'

	' The Player has released the Plunger, Let it go..
	If (KeyCode = GetKeyCode(PlungerKey)) Then
		PlungerTimer.Enabled = False ' stopt den Timer, der den Plunger Stück für Stück zurückzieht (siehe nächste SUB)
		Plunger.LetGo ' lässt den Plunger los und schießt die Kugel ab
		PlungerPercentage = 0 ' setzt den gezogenen Anteil wieder auf '0'
		'FlagPlunger = True
	End If

	'
	' Process The Next set of keys depeding of wether there is a game in progress or not
	'

	' Is A Game in Progress?
	If (fpGameInPlay = TRUE) Then
		' and not tilted
		If (fpTilted = FALSE) Then

			' The Left Flipper Key has been released, Turn Off the Left Flipper(s)
			If (KeyCode = GetKeyCode(LeftFlipperKey)) Then
				LeftFlipper.SolenoidOff
				PlaySound "FlipperDown"
			End If

			' The Right Flipper Key has been released, Turn Off the Right Flipper(s)
			If (KeyCode = GetKeyCode(RightFlipperKey)) Then
				RightFlipper.SolenoidOff
				PlaySound "FlipperDown"
			End If
		End If
	End If
End Sub


Sub PlungerLaneTrigger_Unhit()
	PlaySound "LaunchBall"
End Sub


' The PlungerTimer expired
'
Sub PlungerTimer_Expired()
	PlungerPercentage = PlungerPercentage + 5	' Mit jedem Timer-Start wird der Plunger um 5 % zurückgezogen
	If PlungerPercentage > 100  Then PlungerPercentage = 100	' jedoch mehr als 100 % geht nicht
	Plunger.Pull(PlungerPercentage) ' Zieht den plunger zurück, so lange die Taste gehalten wird
End Sub


' The CoinTimer has expired
'
Sub CoinTimer_Expired()
	CoinTimer.Enabled = False
	PlayMusic 1, "CoinInBell"
	PlaySound "CoinRelais"
	AddCreditTimer.Enabled = True
End Sub


Sub AddCreditTimer_Expired()
	AddCreditTimer.Enabled = False
	PlaySound "CoinRelais"
	FlagCoinLock = False

	If nvCredits < ConstMaxCredits Then nvCredits = nvCredits + 1
	Bb_ApronCredits.State = BulbOn

	If nvCredits  < 10 Then
		DisplayCredits.Text = "0" & nvCredits
	Else
		DisplayCredits.SetValue(nvCredits)
	End If

	If FlagStartGame = False Then Exit Sub

	If FlagStartGame = True and bOnTheFirstBall = False Then
		FlagStartGame = False
		StopMusic 1: PlayMusic 1, "StartGame"
		nvCredits = nvCredits - 1
		If nvCredits = 0 Then Bb_ApronCredits.State = BulbOff

		If nvCredits  < 10 Then
			DisplayCredits.Text = "0" & nvCredits
		Else
			DisplayCredits.SetValue(nvCredits)
		End If

		ResetForNewGame()
	Else
		FlagStartGame = False
		StopMusic 1: PlayMusic 1, "StartGame"
		nvCredits = nvCredits - 1
		If nvCredits = 0 Then Bb_ApronCredits.State = BulbOff
		PlayersPlayingGame = PlayersPlayingGame + 1

		Select Case PlayersPlayingGame
			Case 2
				Display2.Text = "00"
				StopMusic 1: PlayMusic 1, "StartGame"
			Case 3
				Display3.Text = "00"
				StopMusic 1: PlayMusic 1, "StartGame"
			Case 4
				Display4.Text = "00"
				StopMusic 1: PlayMusic 1, "StartGame"
		End Select

		If nvCredits  < 10 Then
			DisplayCredits.Text = "0" & nvCredits
		Else
			DisplayCredits.SetValue(nvCredits)
		End If
	End If
End Sub


' The Played has Nudged the Table a little too hard/much and a Warning
' must be given to the player
'
Sub FuturePinball_TiltWarning(ByVal Warnings)
	'AddDebugText "Tilt Warning" & Warnings

	' play a sound at this point and put something on a display
End Sub


' The Player has tilted the machine (Too Many Warnings)
'
Sub FuturePinball_Tilted()
	Dim i
	'AddDebugText "**Tilted**"

	Bonus = 0
	BonusMultiplier(CurrentPlayer) = 1
	' strore the State of Lane- and Rollover-Lights
	For i = 1 to 5
		If Bb_Lane(i).State = BulbOn Then
			Lane(CurrentPlayer, i) = True
		Else
			Lane(CurrentPlayer, i) = False
		End If
		If Bb_Star(i).State = BulbOn Then
			Star(CurrentPlayer, i) = True
		Else
			Star(CurrentPlayer, i) = False
		End If
		If Bb_LeftLane(i).State = BulbOn Then
			LeftLane(CurrentPlayer, i) = True
		Else
			LeftLane(CurrentPlayer, i) = False
		End If
	Next

	If Bb_LeftUpper3000.State = BulbOn Then
		Lane3000(CurrentPlayer) = True
	Else
		Lane3000(CurrentPlayer) = False
	End If
	If Bb_JetStream.State = BulbOn Then
		JetStream(CurrentPlayer) = True
	Else
		JetStream(CurrentPlayer) = False
	End If
	If Bb_SpecialLeft.State = BulbOn Then
		Special(CurrentPlayer) = "Left"
	ElseIf Bb_SpecialRight.State = BulbOn Then
		Special(CurrentPlayer) = "Right"
	ElseIf Bb_SpecialLeft.State = BulbOff And Bb_SpecialRight.State = BulbOff Then
		Special(CurrentPlayer) = "False"
	End If

	TurnOffLights()
	StopMusic 3
	StopMusic 2
	ScoreCurrentKicker = 0: ScoreNextKicker = 0
	ScoreCurrent5000 = 0: ScoreNext5000 = 0
	ScoreCurrent1000 = 0: ScoreNext1000 = 0
	ScoreCurrent100 = 0: ScoreNext100 = 0
	ScoreCurrent10 = 0: ScoreNext10 = 0
	Score3000 = 0

	' play a sound
	PlaySound "Tilt"
	Bb_Tilt.State = BulbOn

	If nvScore(CurrentPlayer) = 0 Then
		HudDisplay.Text = "00"
		Select Case (CurrentPlayer)
			Case 1: Display1.Text = "00"
			Case 2: Display2.Text = "00"
			Case 3: Display3.Text = "00"
			Case 4: Display4.Text = "00"
		End Select
	End If

	' ensure that the flippers are down (as the keys won't work from now on)
	LeftFlipper.SolenoidOff
	RightFlipper.SolenoidOff
	PlaySound "FlipperDown"

	' you may wish to turn off any lights at this point. (The Light Sequencer
	' will make this very easy)

	' start the tilt recovery timer which waits until all balls have drained
	' before doing the end of ball sequence (or end of game)
	TiltRecoveryTimer.Interval = 2000
	TiltRecoveryTimer.Enabled	= TRUE
End Sub


' A Music Channel has finished Playing.
'
' Channel is set to the channel number that has finished.
'
Sub FuturePinball_MusicFinished(ByVal Channel)
End Sub


' High Score entry has been completed by the player.
'
' Position is set to the position in the high score table (1 - 4)
' if it is set to 0 then there was no new High Score
'
' Special is set to 1 if the Special High Score was beaten
'
Sub FuturePinball_NameEntryComplete(ByVal Position, ByVal Special)
	bEnteringAHighScore = FALSE
	If nvScore(CurrentPlayer) > HighScore Then nvR12 = nvScore(CurrentPlayer)
	If PlayersPlayingGame > 1 And CurrentPlayer < PlayersPlayingGame Then
		EndOfBallComplete()
		Exit Sub
	End If
	EndGame()
End Sub



' *********************************************************************
' **                                                                 **
' **                     User Defined Script Events                  **
' **                                                                 **
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
	Dim i
	Dim p

	'AddDebugText "ResetForNewGame"

	' get Future Pinball to zero out the nvScore (and nvSpecialScore) Variables
	' aswell and ensure the camera is looking in the right direction.
	' this also Sets the fpGameInPlay flag
	BeginGame()

	' increment the total number of games played
   nvTotalGamesPlayed = nvTotalGamesPlayed + 1

	Display1.ResetToZero: Display1.Text = "00"
	Display2.ResetToZero: Display2.Text = "  "
	Display3.ResetToZero: Display3.Text = "  "
	Display4.ResetToZero: Display4.Text = "  "
	HudDisplay.ResetToZero: HudDisplay.Text = "00"

	' increment the total number of games played
   nvTotalGamesPlayed = nvTotalGamesPlayed + 1

	' Start with player 1
	CurrentPlayer = 1

	' Single player (for now, more can be added in later)
	PlayersPlayingGame = 1

	' We are on the First Ball (for Player One)
	bOnTheFirstBall = TRUE

	' initialise all the variables which are used for the duration of the game
	' (do all players incase any new ones start a game)
	For i = 1 To constMaxPlayers
		BonusPoints(i)	= 0
		BonusMultiplier(i) = 1
		BallsRemaining(i) = nvBallsPerGame
		ExtraBallsAwards(i) = 0
		CountLaneLights(i) = 0
		CountLaneComplete(i) = 0
		For p = 1 to 5
			Lane(i, p) = True
			Star(i, p) = False
			LeftLane(i, p) = False
		Next
		JetStream(i) = False
		Lane3000(i) = False
		Special(i) = False
		FlagFreispiel1(i) = False
		FlagFreispiel2(i) = False
		FlagSpecial(i) = False
	Next

	FlagExtraBall = False
	StartAttractTimer.Enabled = False
	AttractHighScoreTimer.Enabled = False
	BreakHighScoreTimer.Enabled = False
	Bb_Tilt.State = BulbOff
	Bb_HighScore.State = BulbOff
	Bb_GameOver1.State = BulbOff
	Bb_GameOver2.State = BulbOff
	For i = 2 to 4
		Bb_Player(i).State = BulbOff
		Bb_PlayerA(i).State = BulbOff
	Next
	TurnOffLights()

	' set up the start delay to handle any Start of Game Attract Sequence
	FirstBallDelayTimer.Interval = 1000
	FirstBallDelayTimer.Enabled = TRUE
End Sub


Sub TurnOffLights()
	Dim i
	Bb_UpperRollover.State = BulbOff
	Bb_LeftUpper3000.State = BulbOff
	Bb_RightUpper3000.State = BulbOff
	For i = 1 to 5
		Bb_Lane(i).State = BulbOff
		Bb_LeftLane(i).State = BulbOff
		Bb_Target(i).State = BulbOff
		Bb_Star(i).State = BulbOff
		Bb_Lane(i).State = BulbOff
	Next
	Bb_JetStream.State = BulbOff
	Bb_OpensGate.State = BulbOff
	Bb_Lites5x.State = BulbOff
	Bb_Lites3x.State = BulbOff
	Bb_Lites2x.State = BulbOff
	Bb_SideTargets3000.State = BulbOff
	Bb_DrpoTargets3000.State = BulbOff
	Bb_LitesExtraBall.State = BulbOff
	Bb_LitesSpecial.State = BulbOff
	Bb_Spinner.State = BulbOff
	Bb_2x.State = BulbOff
	Bb_3x.State = BulbOff
	Bb_5x.State = BulbOff
	Bb_CenterTarget3000.State = BulbOff
	Bb_SpecialLeft.State = BulbOff
	Bb_SpecialRight.State = BulbOff
	Bb_GateOpen.State = BulbOff
	Bumper1.State = BulbOff
	Bumper2.State = BulbOff
	Bumper3.State = BulbOff
	If FlagExtraBall = False Then
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
	End If
	For i = 1 to 10
		Bb_Bonus(i).State = BulbOff
	Next
	Bb_Bonus20000.State = BulbOff
End Sub


' This Timer is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with
'
Sub FirstBallDelayTimer_Expired()
	' stop the timer
	FirstBallDelayTimer.Enabled = FALSE

	' reset the table for a new ball
	ResetForNewPlayerBall()

	' create a new ball in the shooters lane
	CreateNewBall()
End Sub


' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))
'
Sub ResetForNewPlayerBall()
	Dim i
	' make sure the correct display is upto date
	AddScore(0)

	HudDisplay.SetValue(nvScore(CurrentPlayer))

	' reset any drop targets, lights, game modes etc..
	Select Case CurrentPlayer
		Case 1
			If nvScore1 = 0 then
				Display1.QueueText "00", seBlink, 999999, 0, False, ""
				HudDisplay.QueueText "00", seBlink, 999999, 0, False, ""
			Else
				Display1.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
				HudDisplay.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
			End If
		Case 2
			If nvScore2 = 0 then
				Display2.QueueText "00", seBlink, 999999, 0, False, ""
				HudDisplay.QueueText "00", seBlink, 999999, 0, False, ""
			Else
				Display2.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
				HudDisplay.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
			End If
		Case 3
			If nvScore3 = 0 then
				Display3.QueueText "00", seBlink, 999999, 0, False, ""
				HudDisplay.QueueText "00", seBlink, 999999, 0, False, ""
			Else
				Display3.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
				HudDisplay.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
			End If
		Case 4
			If nvScore4 = 0 then
				Display4.QueueText "00", seBlink, 999999, 0, False, ""
				HudDisplay.QueueText "00", seBlink, 999999, 0, False, ""
			Else
				Display4.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
				HudDisplay.QueueText nvScore(CurrentPlayer), seBlink, 999999, 0, False, ""
			End If
	End Select

	HudOVL.Frame CurrentPlayer
	For i = 1 to 4
		Bb_Player(i).State = BulbOff
		'Bb_PlayerA(i).State = BulbOff
	Next
	Bb_Player(CurrentPlayer).State = BulbOn
	'Bb_PlayerA(CurrentPlayer).State = BulbOn

	If nvS1 = "5" Then
		Select Case BallsRemaining(CurrentPlayer)
			Case 5
				DisplayBallInPlay.Text = 1
			Case 4
				DisplayBallInPlay.Text = 2
			Case 3
				DisplayBallInPlay.Text = 3
			Case 2
				DisplayBallInPlay.Text = 4
			Case 1
				DisplayBallInPlay.Text = 5
		End Select
	Else
		Select Case BallsRemaining(CurrentPlayer)
			Case 3
				DisplayBallInPlay.Text = 1
			Case 2
				DisplayBallInPlay.Text = 2
			Case 1
				DisplayBallInPlay.Text = 3
		End Select
	End If

	' reset any drop targets, lights, game modes etc..
	Bb_Match.State = BulbOff
	Bb_BallInPlay.State = BulbOn
	TurnOffLights()
	Bb_UpperRollover.State = BulbOn
	Bumper3.State = BulbOn
	Bb_DrpoTargets3000.State = BulbOn
	Bb_Lites2x.State = BulbOn
	If FlagExtraBall = True Then
		Bb_ShootAgain.State = BulbOn
		SPSA1.State = BulbOn
		SPSA2.State = BulbOn
		SPSA3.State = BulbOn
		SPSA4.State = BulbOn
	End If
	' restore memory lights
	For i = 1 to 5
		If Lane(CurrentPlayer, i) = True Then
			Bb_Lane(i).State = BulbOn
			Bb_Target(i).State = BulbOn
		Else
			Bb_Lane(i).State = BulbOff
			Bb_Target(i).State = BulbOff
		End If
		If Star(CurrentPlayer, i) = True Then
			Bb_Star(i).State = BulbOn
		Else
			Bb_Star(i).State = BulbOff
		End If
		If LeftLane(CurrentPlayer, i) = True Then
			Bb_LeftLane(i).State = BulbOn
		Else
			Bb_LeftLane(i).State = BulbOff
		End If
	Next

	If JetStream(CurrentPlayer) = True Then
		Bb_JetStream.State = BulbOn
		Bb_Star1000.State = BulbOn
	Else
		Bb_JetStream.State = BulbOff
		Bb_Star1000.State = BulbOff
	End If
	If Lane3000(CurrentPlayer) = True Then
		Bb_LeftUpper3000.State = BulbOn
		Bb_RightUpper3000.State = BulbOn
		Bb_CenterTarget3000.State = BulbOn
		Bb_SideTargets3000.State = BulbOn
	Else
		Bb_LeftUpper3000.State = BulbOff
		Bb_RightUpper3000.State = BulbOff
		Bb_CenterTarget3000.State = BulbOff
		Bb_SideTargets3000.State = BulbOff
	End If
	If Special(CurrentPlayer) = "Left" Then
		Bb_SpecialLeft.State = BulbOn
	ElseIf Special(CurrentPlayer) = "Right" Then
		Bb_SpecialRight.State = BulbOn
	ElseIf Special(CurrentPlayer) = "False" Then
		Bb_SpecialLeft.State = BulbOff
		Bb_SpecialRight.State = BulbOff
	End If

	If Bb_Lane(2).State = BulbOn Then Bb_OpensGate.State = BulbOn

	' set the current players bonus multiplier back down to 1X
	BonusMultiplier(CurrentPlayer) = 1
	Bonus = 1
	Bb_Bonus(1).State = BulbOn
	Diverter.SolenoidOff
End Sub


' Create a new ball on the Playfield
'
Sub CreateNewBall()
	' create a ball in the plunger lane kicker.
	PlungerKicker.CreateBall

	' There is a (or another) ball on the playfield
	BallsOnPlayfield = BallsOnPlayfield + 1

	' kick it out..
	PlungerKicker.SolenoidPulse
	PlaySound "CreateBall"
	DropTarget.SolenoidPulse
	'PlaySound "DropTargetReset"
End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
	Dim BonusDelayTime
	Dim i

	'AddDebugText "EndOfBall"

	' the first ball has been lost. From this point on no new players can join in
	bOnTheFirstBall = FALSE

	' strore the State of Lane- and Rollover-Lights
	For i = 1 to 5
		If Bb_Lane(i).State = BulbOn Then
			Lane(CurrentPlayer, i) = True
		Else
			Lane(CurrentPlayer, i) = False
		End If
		If Bb_Star(i).State = BulbOn Then
			Star(CurrentPlayer, i) = True
		Else
			Star(CurrentPlayer, i) = False
		End If
		If Bb_LeftLane(i).State = BulbOn Then
			LeftLane(CurrentPlayer, i) = True
		Else
			LeftLane(CurrentPlayer, i) = False
		End If
	Next

	If Bb_LeftUpper3000.State = BulbOn Then
		Lane3000(CurrentPlayer) = True
	Else
		Lane3000(CurrentPlayer) = False
	End If
	If Bb_JetStream.State = BulbOn Then
		JetStream(CurrentPlayer) = True
	Else
		JetStream(CurrentPlayer) = False
	End If
	If Bb_SpecialLeft.State = BulbOn Then
		Special(CurrentPlayer) = "Left"
	ElseIf Bb_SpecialRight.State = BulbOn Then
		Special(CurrentPlayer) = "Right"
	ElseIf Bb_SpecialLeft.State = BulbOff And Bb_SpecialRight.State = BulbOff Then
		Special(CurrentPlayer) = "False"
	End If

	' only process any of this if the table is not tilted.  (the tilt recovery
	' mechanism will handle any extra balls or end of game)
	If (fpTilted = FALSE) Then
		PlaySound "CoinRelais"
		BonusTemp = Bonus
		BonusMultiplierTemp = BonusMultiplier(CurrentPlayer)
		BonusTimer.Enabled = True
		Select Case Bonus
			Case 1
				PlayMusic 4, "Bonus1000"
			Case 2
				PlayMusic 4, "Bonus2000"
			Case 3
				PlayMusic 4, "Bonus3000"
			Case 4
				PlayMusic 4, "Bonus4000"
			Case 5
				PlayMusic 4, "Bonus5000"
			Case 6
				PlayMusic 4, "Bonus6000"
			Case 7
				PlayMusic 4, "Bonus7000"
			Case 8
				PlayMusic 4, "Bonus8000"
			Case 9
				PlayMusic 4, "Bonus9000"
			Case 10
				PlayMusic 4, "Bonus10000"
			Case 11
				PlayMusic 4, "Bonus11000"
			Case 12
				PlayMusic 4, "Bonus12000"
			Case 13
				PlayMusic 4, "Bonus13000"
			Case 14
				PlayMusic 4, "Bonus14000"
			Case 15
				PlayMusic 4, "Bonus15000"
			Case 16
				PlayMusic 4, "Bonus16000"
			Case 17
				PlayMusic 4, "Bonus17000"
			Case 18
				PlayMusic 4, "Bonus18000"
			Case 19
				PlayMusic 4, "Bonus19000"
			Case 20
				PlayMusic 4, "Bonus20000"
		End Select
		BonusTimer_Expired()
	Else
		' no bonus, so move to the next state quickly
		EndOfBallTimer.Interval = 200
		EndOfBallTimer.Enabled = TRUE
	End If
End Sub


Sub BonusTimer_Expired()
	If Bonus = 0 And BonusMultiplier(CurrentPlayer) = 1 Then
		BonusTimer.Enabled = False
		EndOfBallTimer.Interval = 200
		EndOfBallTimer.Enabled = TRUE
		Exit Sub
	End If

	If Bonus = 20 Then
		Bb_Bonus20000.State = BulbOff
		AddScore(1000)
		'PlayMusic 4, "1000"
		Bonus = Bonus - 1
		Bb_Bonus(10).State = BulbOn
	ElseIf Bonus > 10 And Bonus < 20 then
		Bb_Bonus(Bonus - 10).State = BulbOff
		AddScore(1000)
		'PlayMusic 4, "1000"
		Bonus = Bonus - 1
		If Bonus - 10 > 0 Then Bb_Bonus(Bonus - 10).State = BulbOn
	ElseIf Bonus <= 10 And Bonus > 0 Then
		Bb_Bonus(Bonus).State = BulbOff
		AddScore(1000)
		'PlayMusic 4, "1000"
		Bonus = Bonus - 1
		If Bonus > 0 Then Bb_Bonus(Bonus).State = BulbOn
	End If

	If BonusMultiplier(CurrentPlayer) > 1 And Bonus = 0 Then
		BonusMultiplier(CurrentPlayer) = BonusMultiplier(CurrentPlayer) - 1
		Bonus = BonusTemp
		BonusTimer.Enabled = False
		BonusBreakTimer.Enabled = True
	End If
End Sub

Sub BonusBreakTimer_Expired()
	BonusBreakTimer.Enabled = False

		If Bonus = 20 Then
			Bb_Bonus20000.State = BulbOn
		ElseIf Bonus > 10 Then
			Bb_Bonus(Bonus - 10).State = BulbOn
			Bb_Bonus(10).State = BulbOn
		ElseIf Bonus <= 10 Then
			Bb_Bonus(Bonus).State = BulbOn
		End If

	BonusTimer.Enabled = True
	Select Case Bonus
		Case 1
			PlayMusic 4, "Bonus1000"
		Case 2
			PlayMusic 4, "Bonus2000"
		Case 3
			PlayMusic 4, "Bonus3000"
		Case 4
			PlayMusic 4, "Bonus4000"
		Case 5
			PlayMusic 4, "Bonus5000"
		Case 6
			PlayMusic 4, "Bonus6000"
		Case 7
			PlayMusic 4, "Bonus7000"
		Case 8
			PlayMusic 4, "Bonus8000"
		Case 9
			PlayMusic 4, "Bonus9000"
		Case 10
			PlayMusic 4, "Bonus10000"
		Case 11
			PlayMusic 4, "Bonus11000"
		Case 12
			PlayMusic 4, "Bonus12000"
		Case 13
			PlayMusic 4, "Bonus13000"
		Case 14
			PlayMusic 4, "Bonus14000"
		Case 15
			PlayMusic 4, "Bonus15000"
		Case 16
			PlayMusic 4, "Bonus16000"
		Case 17
			PlayMusic 4, "Bonus17000"
		Case 18
			PlayMusic 4, "Bonus18000"
		Case 19
			PlayMusic 4, "Bonus19000"
		Case 20
			PlayMusic 4, "Bonus20000"
	End Select
	BonusTimer_Expired()
End Sub


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBallTimer_Expired()
	EndOfBallTimer.Enabled = FALSE
	fpTilted = FALSE
	Bb_Tilt.State = BulbOff

	If (ExtraBallsAwards(CurrentPlayer) <> 0) Then
		FlagExtraBall = True
		CreateNewBall()
		ResetForNewPlayerBall()
	Else
		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

		' was that the last ball ?
		If (BallsRemaining(CurrentPlayer) <= 0) Then
			If PlayersPlayingGame > CurrentPlayer Then
				If nvScore(CurrentPlayer) > HighScore Then nvR12 = nvScore(CurrentPlayer)
				If NameEntry = True Then
					bEnteringAHighScore = TRUE
					EnterHighScore(CurrentPlayer)
					Exit Sub
				End If
			End If
			EndOfBallComplete()
		Else
			EndOfBallComplete()
		End If
	End If
End Sub


' This function is called when the end of bonus display
' (or high score entry finished) and it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
   Dim NextPlayer
	Dim x

	'AddDebugText "EndOfBall - Complete"

	' are there multiple players playing this game ?
	If (PlayersPlayingGame > 1) Then
		' then move to the next player
		NextPlayer = CurrentPlayer + 1
		' are we going from the last player back to the first
		' (ie say from player 4 back to player 1)
		If (NextPlayer > PlayersPlayingGame) Then
			NextPlayer = 1
		End If
	Else
		NextPlayer = CurrentPlayer
	End If

	'AddDebugText "Next Player = " & NextPlayer

   ' is it the end of the game ? (all balls been lost for all players)
	If ((BallsRemaining(CurrentPlayer) <= 0) And (BallsRemaining(NextPlayer) <= 0)) Then
		' you may wish to do some sort of Point Match free game award here
		' generally only done when not in free play mode

		MatchNumber = 10 *(Int(Rnd *10))
		MatchPlay = 0

		For x = 1 To PlayersPlayingGame
			If MatchNumber = nvScore(x) Mod 100 Then Replays = Replays + 1
		Next

		If nvScore(CurrentPlayer) > HighScore Then nvR12 = nvScore(CurrentPlayer)

		If nvR12 > HighScore Then
			Replays = Replays + 3
			HighScore = nvR12
		End If

		PlaySound "CoinRelais"
		EndOfGame()
		EndGame
	Else
		' set the next player
		CurrentPlayer = NextPlayer

		' make sure the correct display is upto date
		AddScore(0)

		' reset the playfield for the new player (or new ball)
		ResetForNewPlayerBall()

		' and create a new ball
		CreateNewBall()

	End If
End Sub


' This frunction is called at the End of the Game, it should reset all
' Drop targets, and eject any 'held' balls, start any attract sequences etc..
Sub EndOfGame()
	nvR12 = HighScore
	nvR2 = MatchNumber

	If MatchNumber = 0 Then
		DisplayBallInPlay.Text = "00"
	Else
		DisplayBallInPlay.SetValue(MatchNumber)
	End If

	Bb_Match.State = BulbOn
	Bb_BallInPlay.State = BulbOff
	Bb_GameOver1.State = BulbOn
	Bb_GameOver2.State = BulbOn

	If Replays > 0 Then KnockerTimer.Enabled = True
	PlaySound "CoinRelais"
	PlayMusic 1, "EndGame"

	' ensure that the flippers are down
	LeftFlipper.SolenoidOff
	RightFlipper.SolenoidOff
	StartAttractTimer.Enabled = True
	EnterHighScoreTimer.Enabled = True
	EndGame()

	' you may wish to light any Game Over Light you may have
End Sub


Sub EnterHighscoreTimer_Expired()
	EnterHighscoreTimer.Enabled = False
	If NameEntry = True Then
		bEnteringAHighScore = TRUE
		EnterHighScore(CurrentPlayer)
	End If
End Sub


Sub KnockerTimer_Expired()

	If nvCredits < constMaxCredits Then
		nvCredits = nvCredits + 1
		If nvCredits  < 10 Then
			DisplayCredits.Text = "0" & nvCredits
		Else
			DisplayCredits.SetValue(nvCredits)
		End If
		PlaySound "Knocker"
		Bb_ApronCredits.State = BulbOn
	End If

	Replays = Replays - 1
	If Replays = 0 Then KnockerTimer.Enabled = False
End Sub

Sub StartAttractTimer_Expired()
	Dim i
	StartAttractTimer.Enabled = False
	FlagShowHighScore = False
	AttractHighScoreTimer.Enabled = True

	TurnOffLights()
	SetAllLightsForAttractMode()
End Sub


' The tilt recovery timer waits for all the balls to drain before continuing on
' as per normal
'
Sub TiltRecoveryTimer_Expired()
	' disable the timer
	TiltRecoveryTimer.Enabled	= FALSE
	' if all the balls have been drained then..
	If (BallsOnPlayfield = 0) Then
		' do the normal end of ball thing (this dosn't give a bonus if the table is tilted)
		' the first ball has been lost. From this point on no new players can join in
		bOnTheFirstBall = FALSE

		' no bonus, so move to the next state quickly
		EndOfBallTimer.Interval = 200
		EndOfBallTimer.Enabled = TRUE
	Else
		' else retry (checks again in another second)
		TiltRecoveryTimer.Interval = 10
		TiltRecoveryTimer.Enabled = TRUE
	End If
End Sub


' Set any lights for the Attract Mode.
'
Sub SetAllLightsForAttractMode()
	Dim i
	TurnOffLights()
	Bb_Lane1.BlinkPattern = "011111111111111111111111"
	Bb_Lane3.BlinkPattern = "001111111111111111111111"
	Bb_Lane4.BlinkPattern = "000111111111111111111111"
	Bb_Lane5.BlinkPattern = "000011111111111111111111"
	Bb_Lane2.BlinkPattern = "000001111111111111111111"
	Bb_Target1.BlinkPattern = "011111111111111111111111"
	Bb_Target3.BlinkPattern = "001111111111111111111111"
	Bb_Target4.BlinkPattern = "000111111111111111111111"
	Bb_Target5.BlinkPattern = "000011111111111111111111"
	For i = 1 to 10
		Bb_Bonus(i).State = BulbBlink
	Next
	Bb_Bonus20000.State = BulbBlink
	For i = 1 to 5
		Bb_LeftLane(i).State = BulbBlink
		Bb_Lane(i).State = BulbBlink
		Bb_Target(i).State = BulbBlink
		Bb_Star(i).State = BulbBlink
	Next
	Bb_5x.State = BulbBlink
	Bb_3x.State = BulbBlink
	Bb_2x.State = BulbBlink
	Bb_Spinner.State = BulbBlink
	Bb_JetStream.State = BulbBlink
	Bb_OpensGate.State = BulbBlink
	Bb_CenterTarget3000.State = BulbBlink
	Bb_GateOpen.State = BulbBlink
	Bb_Star1000.State = BulbBlink
	Bb_SideTargets3000.State = BulbBlink
End Sub

' The timer to rotate Score and Highscore gets started
'
Sub AttractHighScoreTimer_Expired()
	If FlagShowHighScore = True Then
		Display1.SetValue(nvR12)
		Display2.SetValue(nvR12)
		Display3.SetValue(nvR12)
		Display4.SetValue(nvR12)
		HudDisplay.SetValue(nvR12)
		Bb_HighScore.State = BulbOn
		FlagShowHighScore = False
		AttractHighScoreTimer.Enabled = False
		BreakHighScoreTimer.Interval = 1500
		BreakHighScoreTimer.Enabled = True
		Exit Sub
	Else
		If nvScore1 = 0 Then
			Display1.Text = "00"
			HudDisplay.Text = "00"
		Else
			Display1.SetValue(nvScore1)
			HudDisplay.SetValue(nvScore(CurrentPlayer))
		End If
		If nvScore2 = 0 Then
			Display2.Text = "00"
		Else
			Display2.SetValue(nvScore2)
		End If
		If nvScore3 = 0 Then
			Display3.Text = "00"
		Else
			Display3.SetValue(nvScore3)
		End If
		If nvScore4 = 0 Then
			Display4.Text = "00"
		Else
			Display4.SetValue(nvScore4)
		End If
		FlagShowHighScore = True
		AttractHighScoreTimer.Enabled = False
		BreakHighScoreTimer.Interval = 3500
		BreakHighScoreTimer.Enabled = True
	End If
End Sub

Sub BreakHighScoreTimer_Expired()
	BreakHighScoreTimer.Enabled = False
	Display1.Text = ""
	HudDisplay.Text = ""
	Display2.Text = ""
	Display3.Text = ""
	Display4.Text = ""
	Bb_HighScore.State = BulbOff
	AttractHighScoreTimer.Enabled = True
End Sub



' *********************************************************************
' **                                                                 **
' **                   Drain / Plunger Functions                     **
' **                                                                 **
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count and test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
	' Destroy the ball
	Drain.DestroyBall
	BallsOnPlayfield = BallsOnPlayfield - 1
	' pretend to knock the ball into the ball storage mech
	PlaySound "Drain"

	' if there is a game in progress and
	If (fpGameInPlay = TRUE) And (fpTilted = FALSE) Then
		' was that the last ball on the playfield
		If (BallsOnPlayfield = 0) Then
			' handle the end of ball (change player, high score entry etc..)
			CheckPointsTimer.Enabled = True
		End If
	End If
End Sub


Sub CheckPointsTimer_Expired()
	If ScoreCurrent5000 = 0 And ScoreCurrent1000 = 0 And ScoreCurrent100 = 0 And ScoreCurrent10 = 0 Then
		CheckPointsTimer.Enabled = False
		EndOfBall()
	End If
End Sub


' A ball is pressing down the trigger in the shooters lane
'
Sub PlungerLaneTrigger_Hit()
	bBallInPlungerLane = TRUE
	Diverter.SolenoidOff
	Bb_GateOpen.State = BulbOff
	Bb_Spinner.State = BulbOff
	If Bb_Lane(2).State = BulbOn Then Bb_OpensGate.State = BulbOn

	' remember last trigger hit by the ball
	set LastSwitchHit = PlungerLaneTrigger
End Sub


' A Ball may of rolled into the Plunger Lane Kicker, if so then kick it
' back out again
'
Sub PlungerKicker_Hit()
	PlungerKicker.SolenoidPulse
End Sub


' The Ball has rolled out of the Plunger Lane.  Check to see if a ball saver machanisim
' is needed and if so fire it up.
'
Sub PlungerLaneGate_Hit()

End Sub


' The ball saver timer has expired.  Turn it off and reset the game flag
'
Sub BallSaverTimer_Expired()
	' stop the timer from repeating
	BallSaverTimer.Enabled = FALSE
	' clear the flag
	bBallSaverActive = FALSE
	' if you have a ball saver light then turn it off at this point
End Sub



' *********************************************************************
' **                                                                 **
' **                   Supporting Score Functions                    **
' **                                                                 **
' *********************************************************************

' Add points to the score and update the score board
'
Sub AddScore(points)
	If (fpTilted = FALSE) Then
		' add the points to the current players score variable
		nvScore(CurrentPlayer) = nvScore(CurrentPlayer) + points

		If nvS1 = "3" Then
			If nvScore(CurrentPlayer) >= 120000 AND nvScore(CurrentPlayer) < 260000 And FlagFreispiel1(CurrentPlayer) = False Then
				If nvCredits < constMaxCredits Then ' jedoch nur, wenn die maximale Anzahl der möglichen Credits noch nicht erreicht wurde
					PlaySound "Knocker" ' Sound wird gespielt
					nvCredits = nvCredits + 1 ' Credits wird um 1 erhöht

					If nvCredits  < 10 Then ' ist die Anzahl der Credits 1-stellig
						DisplayCredits.Text = "0" & nvCredits ' wird diese durch die vorangestellte '0' 2-stellig im Display angezeigt
					Else ' ansonsten ist sie bereits 2-stellig
						DisplayCredits.SetValue(nvCredits) ' und wird angezeigt, wie sie ist
					End If
					Bb_ApronCredits.State = BulbOn
				End If

				FlagFreispiel1(CurrentPlayer) = True ' das Freispiel wurde gegeben und das relevante Flag auf TRUE gesetzt
			ElseIf nvScore(CurrentPlayer) >= 260000 and FlagFreispiel2(CurrentPlayer) = False Then
				If nvCredits < constMaxCredits Then
					PlaySound "Knocker"
					nvCredits = nvCredits + 1

					If nvCredits  < 10 Then
						DisplayCredits.Text = "0" & nvCredits
					Else
						DisplayCredits.SetValue(nvCredits)
					End If
					Bb_ApronCredits.State = BulbOn
				End If

				FlagFreispiel2(CurrentPLayer) = True
			End If
		ElseIf nvS1 = "5" Then
			If nvScore(CurrentPlayer) >= 220000 AND nvScore(CurrentPlayer) < 480000 And FlagFreispiel1(CurrentPlayer) = False Then
				If nvCredits < constMaxCredits Then ' jedoch nur, wenn die maximale Anzahl der möglichen Credits noch nicht erreicht wurde
					PlaySound "Knocker" ' Sound wird gespielt
					nvCredits = nvCredits + 1 ' Credits wird um 1 erhöht

					If nvCredits  < 10 Then ' ist die Anzahl der Credits 1-stellig
						DisplayCredits.Text = "0" & nvCredits ' wird diese durch die vorangestellte '0' 2-stellig im Display angezeigt
					Else ' ansonsten ist sie bereits 2-stellig
						DisplayCredits.SetValue(nvCredits) ' und wird angezeigt, wie sie ist
					End If
					Bb_ApronCredits.State = BulbOn
				End If

				FlagFreispiel1(CurrentPlayer) = True ' das Freispiel wurde gegeben und das relevante Flag auf TRUE gesetzt
			ElseIf nvScore(CurrentPlayer) >= 480000 and FlagFreispiel2(CurrentPlayer) = False Then
				If nvCredits < constMaxCredits Then
					PlaySound "Knocker"
					nvCredits = nvCredits + 1

					If nvCredits  < 10 Then
						DisplayCredits.Text = "0" & nvCredits
					Else
						DisplayCredits.SetValue(nvCredits)
					End If
				End If
				Bb_ApronCredits.State = BulbOn

				FlagFreispiel2(CurrentPLayer) = True
			End If
		End If

		If nvScore(CurrentPlayer) > 999990 Then
			nvScore(CurrentPlayer) = 0
			FlagFreispiel1(CurrentPlayer) = False
			FlagFreispiel2(CurrentPlayer) = False
		End If

		' add the points to the correct display and light the current players display
		Select Case (CurrentPlayer)
			Case 1:	Display1.AddValue(points)
						HudDisplay.AddValue(points)

			Case 2:	Display2.AddValue(points)
						HudDisplay.AddValue(points)

			Case 3:	Display3.AddValue(points)
						HudDisplay.AddValue(points)

			Case 4:	Display4.AddValue(points)
						HudDisplay.AddValue(points)
		End Select
	End if

	' you may wish to check to see if the player has gotten a replay
End Sub


Sub ScoreTimer_Expired()
    !!
    !!VG:bonus score count at end
    !!
	Dim i

	If fpTilted = True Then
		ScoreTimer.Enabled = False
		FlagScore = False
		Exit Sub
	End If

	ScoreCurrentKicker = ScoreCurrentKicker + ScoreNextKicker: ScoreNextKicker = 0
	ScoreCurrent5000 = ScoreCurrent5000 + ScoreNext5000: ScoreNext5000 = 0
	ScoreCurrent1000 = ScoreCurrent1000 + ScoreNext1000: ScoreNext1000 = 0
	ScoreCurrent100 = ScoreCurrent100 + ScoreNext100: ScoreNext100 = 0
	ScoreCurrent10 = ScoreCurrent10 + ScoreNext10: ScoreNext10 = 0

	' all Points are added to the current players score
	If ScoreCurrentKicker = 0 And ScoreCurrent5000 = 0 And ScoreCurrent1000 = 0 And ScoreCurrent100 = 0 And ScoreCurrent10 = 0 And Score3000 = 0 Then
		ScoreTimer.Enabled = False
		FlagScore = False
	End If

	' add kicker points
	If ScoreCurrentKicker > 0 And ScoreCurrent1000 = 0 And ScoreCurrent5000 = 0 And Score3000 = 0 And ScoreCurrent100 = 0 And ScoreCurrent10 = 0 Then
		ScoreTimer.Interval = 230
		WaitScoreTimer.Enabled = True
		AddScore(1000)
		PlayMusic 3, "1000"
		ScoreCurrentKicker = ScoreCurrentKicker - 1000
	End If

	' add 1000 points
	If ScoreCurrent1000 > 0 And ScoreCurrent5000 = 0 And Score3000 = 0 And ScoreCurrent100 = 0 And ScoreCurrent10 = 0 Then
		ScoreTimer.Interval = 150
		AddScore(1000)
		PlayMusic 3, "1000"
		ScoreCurrent1000 = ScoreCurrent1000 - 1000
	End If

	' add 100 points
	If ScoreCurrent100 > 0 And ScoreCurrent5000 = 0 And Score3000 = 0 And ScoreCurrent10 = 0 Then
		ScoreTimer.Interval = 180
		AddScore(100)
		PlayMusic 3, "100"
		ScoreCurrent100 = ScoreCurrent100 - 100
	End If

	' add 5000 points
	If ScoreCurrent5000 > 0 And Score3000 = 0 And ScoreCurrent10 = 0 Then
		ScoreTimer.Interval = 450
		If ScoreCurrent5000 > 999 Then
			AddScore(5000)
			PlayMusic 3, "5000"
			ScoreCurrent5000 = ScoreCurrent5000 - 5000
		Else
			PlayMusic 3, "5000"
			ScoreCurrent5000 = ScoreCurrent5000 - 1
		End If
	End If

	' add 10 points
	If ScoreCurrent10 > 0 And Score3000 = 0 Then
		If LastSwitchHit.Name <> "Kicker" Then ScoreTimer.Interval = 180
		AddScore(10)
		PlayMusic 3, "10"
		ScoreCurrent10 = ScoreCurrent10 - 10
	End If

	' add 3000 Points from Droptarget reset
	If Score3000 > 0 Then
		ScoreTimer.Interval = 150
		AddScore(1000)
		PlayMusic 3, "1000"
		Score3000 = Score3000 - 1000
		If Score3000 = 0 Then DropTargetResetTimer.Enabled = True
	End If
End Sub

' *********************************************************************
' **                                                                 **
' **                     Table Object Script Events                  **
' **                                                                 **
' *********************************************************************

Sub Alternate()
	If Bumper1.State = BulbOn Then
		Bumper1.State = BulbOff
		Bumper2.State = BulbOff
		Bumper3.State = BulbOn
	Else
		Bumper3.State = BulbOff
		Bumper1.State = BulbOn
		Bumper2.State = BulbOn
	End If

	If FlagSpecial(CurrentPlayer) = True Then
		If Bb_SpecialLeft.State = BulbOn Then
			Bb_SpecialLeft.State = BulbOff
			Bb_SpecialRight.State = BulbOn
		Else
			Bb_SpecialRight.State = BulbOff
			Bb_SpecialLeft.State = BulbOn
		End If
	End If
End Sub

' *********************************************************************

Sub AdvanceBonusTimer_Expired()
	If fpTilted = True Then Exit Sub

	If Bonus = 20 Or BonusStep = 0 Then
		AdvanceBonusTimer.Enabled = False
		Exit Sub
	End If

	If BonusStep > 0 Then BonusStep = BonusStep - 1

	If Bonus < 10 Then
		Bb_Bonus(Bonus).State = BulbOff
		Bonus = Bonus + 1
		Bb_Bonus(Bonus).State = BulbOn
		Exit Sub
	End If

	If Bonus > 10 And Bonus < 20 Then
		Bb_Bonus(Bonus - 10).State = BulbOff
		Bonus = Bonus + 1
		Bb_Bonus(Bonus -10).State = BulbOn
	End If

	If Bonus = 20 Then
		Bb_Bonus(10).State = BulbOff
		Bb_Bonus(9).State = BulbOff
		Bb_Bonus20000.State = BulbOn
	End If

	If Bonus = 10 Then
		Bonus = Bonus + 1
		Bb_Bonus(1).State = BulbOn
	End If
End Sub

' *********************************************************************

Sub UpperRollover_Hit()
	If fpTilted = True Then Exit Sub
	Alternate()

	If FlagScore = False Then
		PlayMusic 2, "5000"
		FlagScore = True
		ScoreTimer.Enabled = False
		StopScoreTimer.Enabled = True
		BonusStep = 3
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If

	ScoreNext100 = ScoreNext100 + 300
End Sub

' *********************************************************************

Sub StopScoreTimer_Expired()
	StopScoreTimer.Enabled = False
	FlagScore = False
	ScoreTimer.Enabled = True
	ScoreTimer_Expired()
End Sub

' *********************************************************************

Sub Lane1_Hit()
	If fpTilted = True Then Exit Sub

	Dim i
	Alternate()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_LeftUpper3000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 300
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 300
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 3000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 3000
		End If
	End If

	'If nvS1 = "3" Then
	'	If Bb_Lane(1).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 2
	'	Bb_Lane(1).State = BulbOff
	'	Bb_Lane(3).State = BulbOff
	'	Bb_Target1.State = BulbOff
	'	Bb_Target3.State = BulbOff
	'	If CountLaneComplete(CurrentPlayer) = 0 Then
	'		Bb_Star(1).State = BulbOn
	'		Bb_Star(3).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
	'		Bb_LeftLane(1).State = BulbOn
	'		Bb_leftLane(3).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_LeftUpper3000.State = BulbOn
	'		Bb_RightUpper3000.State = BulbOn
	'		Bb_SideTargets3000.State = BulbOn
	'		Bb_CenterTarget3000.State = BulbOn
	'		Bb_SpecialLeft.State = BulbOn
	'		'Bb_SpecialRight.State = BulbOn
	'		FlagSpecial(CurrentPlayer) = True
	'	ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_Star1000.State = BulbOn
	'		Bb_JetStream.State = BulbOn
	'		If FlagScore = False Then
	'			FlagScore = True
	'			ScoreCurrent5000 = 25000
	'			ScoreTimer.Enabled = True
	'			ScoreTimer_Expired()
	'		Else
	'			ScoreNext5000 = ScoreNext5000 + 25000
	'		End If
	'	End If
	'Else
		If Bb_Lane(1).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
		Bb_Lane(1).State = BulbOff
		Bb_Target1.State = BulbOff
		If CountLaneComplete(CurrentPlayer) = 0 Then
			Bb_Star(1).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
			Bb_LeftLane(1).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_LeftUpper3000.State = BulbOn
			Bb_RightUpper3000.State = BulbOn
			Bb_SideTargets3000.State = BulbOn
			Bb_CenterTarget3000.State = BulbOn
			Bb_SpecialLeft.State = BulbOn
			'Bb_SpecialRight.State = BulbOn
			FlagSpecial(CurrentPlayer) = True
		ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_Star1000.State = BulbOn
			Bb_JetStream.State = BulbOn
			If FlagScore = False Then
				FlagScore = True
				ScoreCurrent5000 = 25000
				ScoreTimer.Enabled = True
				ScoreTimer_Expired()
			Else
				ScoreNext5000 = ScoreNext5000 + 25000
			End If
		End If
	'End If

	If CountLaneLights(CurrentPlayer) = 5 Then
		'If CountLaneComplete(CurrentPlayer) < 3 Then
			For i = 1 to 5
				Bb_Lane(i).FlashForMs 2000,200,BulbOn
			Next
			For i = 1 to 5
				Bb_Target(i).FlashForMS 2000,200,BulbOn
			Next
			If Bb_GateOpen.State = BulbOff Then Bb_OpensGate.State = BulbOn
			If CountLaneComplete(CurrentPlayer) < 3 Then
				If FlagScore = False Then
					FlagScore = True
					ScoreCurrent5000 = 5
					ScoreTimer.Enabled = True
					ScoreTimer_Expired()
				Else
					ScoreNext5000 = ScoreNext5000 + 5
				End If
			End If
			CountLaneComplete(CurrentPlayer) = CountLaneComplete(CurrentPlayer) + 1
			If CountLaneComplete(CurrentPlayer) > 3 Then CountLaneComplete(CurrentPlayer) = 3
		'End If
		CountLaneLights(CurrentPlayer) = 0

	End If
End Sub

Sub Lane2_Hit()
	If fpTilted = True Then Exit Sub

	Dim i
	Alternate()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_Lane(2).State = BulbOn Then
		Bb_Spinner.State = BulbOn
		Diverter.SolenoidOn
		Bb_OpensGate.State = BulbOff
		Bb_GateOpen.State = BulbOn
	End If

	If Bb_LeftUpper3000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 300
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 300
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 3000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 3000
		End If
	End If

	'If nvS1 = "3" Then
	'	If Bb_Lane(2).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
	'	Bb_Lane(2).State = BulbOff
	'	If CountLaneComplete(CurrentPlayer) = 0 Then
	'		Bb_Star(2).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
	'		Bb_LeftLane(2).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_LeftUpper3000.State = BulbOn
	'		Bb_RightUpper3000.State = BulbOn
	'		Bb_SideTargets3000.State = BulbOn
	'		Bb_CenterTarget3000.State = BulbOn
	'		Bb_SpecialLeft.State = BulbOn
	'		'Bb_SpecialRight.State = BulbOn
	'		FlagSpecial(CurrentPlayer) = True
	'	ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_Star1000.State = BulbOn
	'		Bb_JetStream.State = BulbOn
	'		If FlagScore = False Then
	'			FlagScore = True
	'			ScoreCurrent5000 = 25000
	'			ScoreTimer.Enabled = True
	'			ScoreTimer_Expired()
	'		Else
	'			ScoreNext5000 = ScoreNext5000 + 25000
	'		End If
	'	End If
	'Else
		If Bb_Lane(2).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
		Bb_Lane(2).State = BulbOff
		If CountLaneComplete(CurrentPlayer) = 0 Then
			Bb_Star(2).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
			Bb_LeftLane(2).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_LeftUpper3000.State = BulbOn
			Bb_RightUpper3000.State = BulbOn
			Bb_SideTargets3000.State = BulbOn
			Bb_CenterTarget3000.State = BulbOn
			Bb_SpecialLeft.State = BulbOn
			'Bb_SpecialRight.State = BulbOn
			FlagSpecial(CurrentPlayer) = True
		ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_Star1000.State = BulbOn
			Bb_JetStream.State = BulbOn
			If FlagScore = False Then
				FlagScore = True
				ScoreCurrent5000 = 25000
				ScoreTimer.Enabled = True
				ScoreTimer_Expired()
			Else
				ScoreNext5000 = ScoreNext5000 + 25000
			End If
		End If
	'End If

	If CountLaneLights(CurrentPlayer) = 5 Then
		'If CountLaneComplete(CurrentPlayer) < 3 Then
			For i = 1 to 5
				Bb_Lane(i).FlashForMs 2000,200,BulbOn
			Next
			For i = 1 to 5
				Bb_Target(i).FlashForMS 2000,200,BulbOn
			Next
			If Bb_GateOpen.State = BulbOff Then Bb_OpensGate.State = BulbOn
			If CountLaneComplete(CurrentPlayer) < 3 Then
				If FlagScore = False Then
					FlagScore = True
					ScoreCurrent5000 = 5
					ScoreTimer.Enabled = True
					ScoreTimer_Expired()
				Else
					ScoreNext5000 = ScoreNext5000 + 5
				End If
			End If
			CountLaneComplete(CurrentPlayer) = CountLaneComplete(CurrentPlayer) + 1
			If CountLaneComplete(CurrentPlayer) > 3 Then CountLaneComplete(CurrentPlayer) = 3
		'End If
		CountLaneLights(CurrentPlayer) = 0

	End If
End Sub

Sub Lane3_Hit()
	If fpTilted = True Then Exit Sub

	Dim i
	Alternate()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_LeftUpper3000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 300
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 300
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 3000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 3000
		End If
	End If

	'If nvS1 = "3" Then
	'	If Bb_Lane(3).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 2
	'	Bb_Lane(1).State = BulbOff
	'	Bb_Lane(3).State = BulbOff
	'	Bb_Target1.State = BulbOff
	'	Bb_Target3.State = BulbOff
	'	If CountLaneComplete(CurrentPlayer) = 0 Then
	'		Bb_Star(1).State = BulbOn
	'		Bb_Star(3).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
	'		Bb_LeftLane(1).State = BulbOn
	'		Bb_leftLane(3).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_LeftUpper3000.State = BulbOn
	'		Bb_RightUpper3000.State = BulbOn
	'		Bb_SideTargets3000.State = BulbOn
	'		Bb_CenterTarget3000.State = BulbOn
	'		Bb_SpecialLeft.State = BulbOn
	'		'Bb_SpecialRight.State = BulbOn
	'		FlagSpecial(CurrentPlayer) = True
	'	ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_Star1000.State = BulbOn
	'		Bb_JetStream.State = BulbOn
	'		If FlagScore = False Then
	'			FlagScore = True
	'			ScoreCurrent5000 = 25000
	'			ScoreTimer.Enabled = True
	'			ScoreTimer_Expired()
	'		Else
	'			ScoreNext5000 = ScoreNext5000 + 25000
	'		End If
	'	End If
	'Else
		If Bb_Lane(3).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
		Bb_Lane(3).State = BulbOff
		Bb_Target3.State = BulbOff
		If CountLaneComplete(CurrentPlayer) = 0 Then
			Bb_Star(3).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
			Bb_LeftLane(3).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_LeftUpper3000.State = BulbOn
			Bb_RightUpper3000.State = BulbOn
			Bb_SideTargets3000.State = BulbOn
			Bb_CenterTarget3000.State = BulbOn
			Bb_SpecialLeft.State = BulbOn
			'Bb_SpecialRight.State = BulbOn
			FlagSpecial(CurrentPlayer) = True
		ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_Star1000.State = BulbOn
			Bb_JetStream.State = BulbOn
			If FlagScore = False Then
				FlagScore = True
				ScoreCurrent5000 = 25000
				ScoreTimer.Enabled = True
				ScoreTimer_Expired()
			Else
				ScoreNext5000 = ScoreNext5000 + 25000
			End If
		End If
	'End If

	If CountLaneLights(CurrentPlayer) = 5 Then
		'If CountLaneComplete(CurrentPlayer) < 3 Then
			For i = 1 to 5
				Bb_Lane(i).FlashForMs 2000,200,BulbOn
			Next
			For i = 1 to 5
				Bb_Target(i).FlashForMS 2000,200,BulbOn
			Next
			If Bb_GateOpen.State = BulbOff Then Bb_OpensGate.State = BulbOn
			If CountLaneComplete(CurrentPlayer) < 3 Then
				If FlagScore = False Then
					FlagScore = True
					ScoreCurrent5000 = 5
					ScoreTimer.Enabled = True
					ScoreTimer_Expired()
				Else
					ScoreNext5000 = ScoreNext5000 + 5
				End If
			End If
			CountLaneComplete(CurrentPlayer) = CountLaneComplete(CurrentPlayer) + 1
			If CountLaneComplete(CurrentPlayer) > 3 Then CountLaneComplete(CurrentPlayer) = 3
		'End If
		CountLaneLights(CurrentPlayer) = 0

	End If
End Sub

Sub Lane4_Hit()
	If fpTilted = True Then Exit Sub

	Dim i
	Alternate()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_LeftUpper3000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 300
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 300
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 3000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 3000
		End If
	End If

	'If nvS1 = "3" Then
	'	If Bb_Lane(4).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 2
	'	Bb_Lane(4).State = BulbOff
	'	Bb_Lane(5).State = BulbOff
	'	Bb_Target4.State = BulbOff
	'	Bb_Target5.State = BulbOff
	'	If CountLaneComplete(CurrentPlayer) = 0 Then
	'		Bb_Star(4).State = BulbOn
	'		Bb_Star(5).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
	'		Bb_LeftLane(4).State = BulbOn
	'		Bb_leftLane(5).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_LeftUpper3000.State = BulbOn
	'		Bb_RightUpper3000.State = BulbOn
	'		Bb_SideTargets3000.State = BulbOn
	'		Bb_CenterTarget3000.State = BulbOn
	'		Bb_SpecialLeft.State = BulbOn
	'		'Bb_SpecialRight.State = BulbOn
	'		FlagSpecial(CurrentPlayer) = True
	'	ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_Star1000.State = BulbOn
	'		Bb_JetStream.State = BulbOn
	'		If FlagScore = False Then
	'			FlagScore = True
	'			ScoreCurrent5000 = 25000
	'			ScoreTimer.Enabled = True
	'			ScoreTimer_Expired()
	'		Else
	'			ScoreNext5000 = ScoreNext5000 + 25000
	'		End If
	'	End If
	'Else
		If Bb_Lane(4).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
		Bb_Lane(4).State = BulbOff
		Bb_Target4.State = BulbOff
		If CountLaneComplete(CurrentPlayer) = 0 Then
			Bb_Star(4).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
			Bb_LeftLane(4).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_LeftUpper3000.State = BulbOn
			Bb_RightUpper3000.State = BulbOn
			Bb_SideTargets3000.State = BulbOn
			Bb_CenterTarget3000.State = BulbOn
			Bb_SpecialLeft.State = BulbOn
			'Bb_SpecialRight.State = BulbOn
			FlagSpecial(CurrentPlayer) = True
		ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_Star1000.State = BulbOn
			Bb_JetStream.State = BulbOn
			If FlagScore = False Then
				FlagScore = True
				ScoreCurrent5000 = 25000
				ScoreTimer.Enabled = True
				ScoreTimer_Expired()
			Else
				ScoreNext5000 = ScoreNext5000 + 25000
			End If
		End If
	'End If

	If CountLaneLights(CurrentPlayer) = 5 Then
		'If CountLaneComplete(CurrentPlayer) < 3 Then
			For i = 1 to 5
				Bb_Lane(i).FlashForMs 2000,200,BulbOn
			Next
			For i = 1 to 5
				Bb_Target(i).FlashForMS 2000,200,BulbOn
			Next
			If Bb_GateOpen.State = BulbOff Then Bb_OpensGate.State = BulbOn
			If CountLaneComplete(CurrentPlayer) < 3 Then
				If FlagScore = False Then
					FlagScore = True
					ScoreCurrent5000 = 5
					ScoreTimer.Enabled = True
					ScoreTimer_Expired()
				Else
					ScoreNext5000 = ScoreNext5000 + 5
				End If
			End If
			CountLaneComplete(CurrentPlayer) = CountLaneComplete(CurrentPlayer) + 1
			If CountLaneComplete(CurrentPlayer) > 3 Then CountLaneComplete(CurrentPlayer) = 3
		'End If
		CountLaneLights(CurrentPlayer) = 0

	End If
End Sub

Sub Lane5_Hit()
	If fpTilted = True Then Exit Sub

	Dim i
	Alternate()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_LeftUpper3000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 300
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 300
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 3000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 3000
		End If
	End If

	'If nvS1 = "3" Then
	'	If Bb_Lane(4).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 2
	'	Bb_Lane(4).State = BulbOff
	'	Bb_Lane(5).State = BulbOff
	'	Bb_Target4.State = BulbOff
	'	Bb_Target5.State = BulbOff
	'	If CountLaneComplete(CurrentPlayer) = 0 Then
	'		Bb_Star(4).State = BulbOn
	'		Bb_Star(5).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
	'		Bb_LeftLane(4).State = BulbOn
	'		Bb_leftLane(5).State = BulbOn
	'	ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_LeftUpper3000.State = BulbOn
	'		Bb_RightUpper3000.State = BulbOn
	'		Bb_SideTargets3000.State = BulbOn
	'		Bb_CenterTarget3000.State = BulbOn
	'		Bb_SpecialLeft.State = BulbOn
	'		'Bb_SpecialRight.State = BulbOn
	'		FlagSpecial(CurrentPlayer) = True
	'	ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
	'		Bb_Star1000.State = BulbOn
	'		Bb_JetStream.State = BulbOn
	'		If FlagScore = False Then
	'			FlagScore = True
	'			ScoreCurrent5000 = 25000
	'			ScoreTimer.Enabled = True
	'			ScoreTimer_Expired()
	'		Else
	'			ScoreNext5000 = ScoreNext5000 + 25000
	'		End If
	'	End If
	'Else
		If Bb_Lane(5).State = BulbOn Then CountLaneLights(CurrentPlayer) = CountLaneLights(CurrentPlayer) + 1
		Bb_Lane(5).State = BulbOff
		Bb_Target5.State = BulbOff
		If CountLaneComplete(CurrentPlayer) = 0 Then
			Bb_Star(5).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 1 Then
			Bb_LeftLane(5).State = BulbOn
		ElseIf CountLaneComplete(CurrentPlayer) = 2 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_LeftUpper3000.State = BulbOn
			Bb_RightUpper3000.State = BulbOn
			Bb_SideTargets3000.State = BulbOn
			Bb_CenterTarget3000.State = BulbOn
			Bb_SpecialLeft.State = BulbOn
			'Bb_SpecialRight.State = BulbOn
			FlagSpecial(CurrentPlayer) = True
		ElseIf CountLaneComplete(CurrentPlayer) = 3 And CountLaneLights(CurrentPlayer) = 5 Then
			Bb_Star1000.State = BulbOn
			Bb_JetStream.State = BulbOn
			If FlagScore = False Then
				FlagScore = True
				ScoreCurrent5000 = 25000
				ScoreTimer.Enabled = True
				ScoreTimer_Expired()
			Else
				ScoreNext5000 = ScoreNext5000 + 25000
			End If
		End If
	'End If

	If CountLaneLights(CurrentPlayer) = 5 Then
		'If CountLaneComplete(CurrentPlayer) < 3 Then
			For i = 1 to 5
				Bb_Lane(i).FlashForMs 2000,200,BulbOn
			Next
			For i = 1 to 5
				Bb_Target(i).FlashForMS 2000,200,BulbOn
			Next
			If Bb_GateOpen.State = BulbOff Then Bb_OpensGate.State = BulbOn
			If CountLaneComplete(CurrentPlayer) < 3 Then
				If FlagScore = False Then
					FlagScore = True
					ScoreCurrent5000 = 5
					ScoreTimer.Enabled = True
					ScoreTimer_Expired()
				Else
					ScoreNext5000 = ScoreNext5000 + 5
				End If
			End If
			CountLaneComplete(CurrentPlayer) = CountLaneComplete(CurrentPlayer) + 1
			If CountLaneComplete(CurrentPlayer) > 3 Then CountLaneComplete(CurrentPlayer) = 3
		'End If
		CountLaneLights(CurrentPlayer) = 0

	End If
End Sub

' *********************************************************************

Sub DropTarget_Hit()
	If fpTilted = True Then Exit Sub
	Alternate()

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent100 = 300
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext100 = ScoreNext100 + 300
	End If

	BonusStep = 1
	AdvanceBonusTimer.Enabled = True
	AdvanceBonusTimer_Expired()

	If (DropTarget.Dropped = True) And Bb_LitesSpecial.State = BulbOn Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent5000 = 25000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext5000 = ScoreNext5000 + 25000
		End If
		DropTargetResetTimer.Enabled = True
	ElseIf (DropTarget.Dropped = True) And Bb_LitesExtraBall.State = BulbOn Then
		Bb_LitesExtraBall.State = BulbOff
		Bb_ShootAgain.State = BulbOn
		SPSA1.State = BulbOn
		SPSA2.State = BulbOn
		SPSA3.State = BulbOn
		SPSA4.State = BulbOn
		Bb_LitesSpecial.State = BulbOn
		ExtraBallsAwards(CurrentPlayer) = 1
		DropTargetResetTimer.Enabled = True
	ElseIf (DropTarget.Dropped = True) And Bb_DrpoTargets3000.State = BulbOn Then
		Bb_DrpoTargets3000.State = BulbOff
		Bb_LitesExtraBall.State = BulbOn
		Score3000 = 3000
	End If
End Sub

Sub DropTargetResetTimer_Expired()
	DropTargetResetTimer.Enabled = False
	Droptarget.SolenoidPulse
	PlaySound "DropTargetReset"
End Sub

Sub DropTargetRubber_Hit()
	If fpTilted = True Then Exit Sub

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent10 = 10
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext10 = ScoreNext10 + 10
	End If
End Sub

' *********************************************************************

Sub Spinner_Hit()
	If FlagExtraBall = True Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		Bb_ShootAgain.State = BulbOff
		SPSA1.State = BulbOff
		SPSA2.State = BulbOff
		SPSA3.State = BulbOff
		SPSA4.State = BulbOff
		FlagExtraBall = False
	End If

	If Bb_Spinner.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If
End Sub

' *********************************************************************

Sub Kicker_Hit()
	PlaySound "Drain"
	If fpTilted = True Then Exit Sub

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrentKicker = 3000
		ScoreTimer.Interval = 230
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreCurrentKicker = ScoreCurrentKicker + 3000
	End If
End Sub

Sub WaitScoreTimer_Expired()
	WaitScoreTimer.Enabled = False
	PlayMusic 2, "MusicKicker"
	BonusStep = 1
	AdvanceBonusTimer.Enabled = True
	AdvanceBonusTimer_Expired()
	KickerTimer.Enabled = True
End Sub

Sub KickerTimer_Expired()
	KickerTimer.Enabled = False
	Set LastSwitchHit = DummyTrigger
	If Bb_Lites2x.State = BulbOn Then
		Bb_Lites2x.State = BulbOff
		Bb_2x.State = BulbOn
		Bb_Lites3x.State = BulbOn
		BonusMultiplier(CurrentPlayer) = 2
	ElseIf Bb_Lites3x.State = BulbOn Then
		Bb_Lites3x.State = BulbOff
		Bb_2x.State = BulbOff
		Bb_3x.State = BulbOn
		Bb_Lites5x.State = BulbOn
		BonusMultiplier(CurrentPlayer) = 3
	ElseIf Bb_Lites5x.State = BulbOn Then
		Bb_Lites5x.State = BulbOff
		Bb_3x.State = BulbOff
		Bb_5x.State = BulbOn
		BonusMultiplier(CurrentPlayer) = 5
	End If
	Kicker.SolenoidPulse
	PlaySound "Kicker"
End Sub

' *********************************************************************

Sub Bumper1_Hit()
	If fpTilted = True Then Exit Sub

	If Bumper1.State = BulbOn Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent10 = 10
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext10 = ScoreNext10 + 10
		End If
	End If

	PB11.SolenoidPulse
	PB12.SolenoidPulse
	PB13.SolenoidPulse
	PB14.SolenoidPulse
	PB15.SolenoidPulse
	PB16.SolenoidPulse
	PB17.SolenoidPulse
	PB18.SolenoidPulse

	PlaySound "Bumper"
End Sub

Sub Bumper2_Hit()
	If fpTilted = True Then Exit Sub

	If Bumper2.State = BulbOn Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent10 = 10
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext10 = ScoreNext10 + 10
		End If
	End If

	PB21.SolenoidPulse
	PB22.SolenoidPulse
	PB23.SolenoidPulse
	PB24.SolenoidPulse
	PB25.SolenoidPulse
	PB26.SolenoidPulse
	PB27.SolenoidPulse
	PB28.SolenoidPulse

	PlaySound "Bumper"
End Sub

Sub Bumper3_Hit()
	If fpTilted = True Then Exit Sub

	If Bumper3.State = BulbOn Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent10 = 10
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext10 = ScoreNext10 + 10
		End If
	End If

	PB31.SolenoidPulse
	PB32.SolenoidPulse
	PB33.SolenoidPulse
	PB34.SolenoidPulse
	PB35.SolenoidPulse
	PB36.SolenoidPulse
	PB37.SolenoidPulse
	PB38.SolenoidPulse

	PlaySound "Bumper"
End Sub

' *********************************************************************

Sub Target1_Hit()
	PlaySound "TargetHit"
	If fpTilted = True Then Exit Sub
	Lane1_Hit()
End Sub

Sub Target3_Hit()
	PlaySound "TargetHit"
	If fpTilted = True Then Exit Sub
	Lane3_Hit()
End Sub

Sub Target4_Hit()
	PlaySound "TargetHit"
	Lane4_Hit()
End Sub

Sub Target5_Hit()
	PlaySound "TargetHit"
	If fpTilted = True Then Exit Sub
	Lane5_Hit()
End Sub

' *********************************************************************

Sub StarRollover1_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_Star1000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_Star1.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub StarRollover2_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_Star1000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_Star2.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub StarRollover3_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_Star1000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_Star3.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub StarRollover4_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_Star1000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_Star4.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub StarRollover5_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_Star1000.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_Star5.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

' *********************************************************************

Sub LaneRollover1_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_JetStream.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_LeftLane1.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub LaneRollover2_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_JetStream.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_LeftLane2.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub LaneRollover3_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_JetStream.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_LeftLane3.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub LaneRollover4_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_JetStream.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_LeftLane4.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

Sub LaneRollover5_Hit()
	If fpTilted = True Then Exit Sub

	If Bb_JetStream.State = BulbOff Then
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent100 = 100
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext100 = ScoreNext100 + 100
		End If
	Else
		If FlagScore = False Then
			FlagScore = True
			ScoreCurrent1000 = 1000
			ScoreTimer.Enabled = True
			ScoreTimer_Expired()
		Else
			ScoreNext1000 = ScoreNext1000 + 1000
		End If
	End If

	If Bb_LeftLane5.State = BulbOn Then
		BonusStep = 1
		AdvanceBonusTimer.Enabled = True
		AdvanceBonusTimer_Expired()
	End If
End Sub

' *********************************************************************

Sub LeftSlingshot_Hit()
	If fpTilted = True Then Exit Sub

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent10 = 10
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext10 = ScoreNext10 + 10
	End If
	PlaySound "Slingshot"
End Sub

Sub RightSlingshot_Hit()
	If fpTilted = True Then Exit Sub
	LeftSlingshot_Hit()
End Sub

' *********************************************************************

Sub LeftInlane_Hit()
	If fpTilted = True Then Exit Sub
	Alternate()

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent100 = 300
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext100 = ScoreNext100 + 300
	End If
End Sub

Sub RightInLane_Hit()
	If fpTilted = True Then Exit Sub
	LeftInlane_Hit()
End Sub

Sub LeftOutlane_Hit()
	If fpTilted = True Then Exit Sub

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent1000 = 1000
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext1000 = ScoreNext1000 + 1000
	End If

	If Bb_SpecialLeft.State = BulbOn Then
		Bb_SpecialLeft.State = BulbOff
		FlagSpecial(CurrentPlayer) = False
		If nvCredits < constMaxCredits Then
			PlaySound "Knocker"
			nvCredits = nvCredits + 1
			If nvCredits  < 10 Then
				DisplayCredits.Text = "0" & nvCredits
			Else
				DisplayCredits.SetValue(nvCredits)
			End If
		End If
		Bb_ApronCredits.State = BulbOn
	End If
End Sub

Sub RightOutlane_Hit()
	If fpTilted = True Then Exit Sub

	If FlagScore = False Then
		FlagScore = True
		ScoreCurrent1000 = 1000
		ScoreTimer.Enabled = True
		ScoreTimer_Expired()
	Else
		ScoreNext1000 = ScoreNext1000 + 1000
	End If

	If Bb_SpecialRight.State = BulbOn Then
		Bb_SpecialRight.State = BulbOff
		FlagSpecial(CurrentPlayer) = False
		If nvCredits < constMaxCredits Then
			PlaySound "Knocker"
			nvCredits = nvCredits + 1
			If nvCredits  < 10 Then
				DisplayCredits.Text = "0" & nvCredits
			Else
				DisplayCredits.SetValue(nvCredits)
			End If
		End If
		Bb_ApronCredits.State = BulbOn
	End If
End Sub

' *********************************************************************

Sub SurfaceRightLane_Hit()
	PlaySound "Wall"
End Sub

Sub SurfaceLeftLane_Hit()
	PlaySound "Wall"
End Sub

' *********************************************************************

Sub LeftBorder_Hit()
	PlaySound "Holz"
End Sub

Sub MiddleBorder_Hit()
	PlaySound "Holz"
End Sub

Sub RightBorder_Hit()
	PlaySound "Holz"
End Sub

Sub LeftApron_Hit()
	PlaySound "ApronHit"
End Sub

Sub RightApron_Hit()
	PlaySound "ApronHit"
End Sub

Sub LeftInlaneGuide_Hit()
	PlaySound "Wall"
End Sub

Sub RightInlaneGuide_Hit()
	PlaySound "Wall"
End Sub

Sub LeftApronRoll_Hit()
	PlayMusic 1, "ApronRoll"
End Sub

Sub RightApronRoll_Hit()
	PlayMusic 1, "ApronRoll"
End Sub


