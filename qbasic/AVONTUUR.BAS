'$DYNAMIC
DECLARE SUB LeesAvontuur (BestandsNaam$)
DECLARE SUB Muteer (Plek%, Bewerking$, Waarde%)
DECLARE SUB VoerActieUit (Actie$)
DECLARE SUB ToonActies ()
DECLARE FUNCTION Toets! (Plek%, Bewerking$, Waarde%)
DECLARE FUNCTION Toegestaan% (Bewering$)
DECLARE SUB ToonGebeurtenis ()
DECLARE SUB Tekst (Kleur%, Regel$)
DECLARE FUNCTION Interpoleer$ (Zin$)

' Zorg dat we spel kunnen afsluiten met ESC
KEY 15, CHR$(0) + CHR$(1) ' esc
KEY(15) ON
ON KEY(15) GOSUB Stoppen

KEY 16, CHR$(0) + CHR$(31) ' s
ON KEY(16) GOSUB Opslaan
ON ERROR GOTO Foutafhandeling

Fout = 0
DIM SHARED Naam$
DIM SHARED Gebeurtenissen(100) AS INTEGER
DIM SHARED HuidigSpel%
DIM SHARED Overslaan%

DIM SHARED TekstLengte
DIM SHARED StartActies
DIM SHARED Teksten(20000) AS STRING

PRINT "Laden..."
LeesAvontuur "AVONTUUR.DAT"

'CLS
LOCATE 5, 20
PRINT "Avontuur!"

AantalOpslag% = 0

FOR i% = 1 TO 5
  SpelOpslag$ = "OPSLAG" + STR$(i%) + ".SAV"
  OPEN SpelOpslag$ FOR INPUT AS 1
  IF Fout = 0 THEN
    AantalOpslag% = AantalOpslag% + 1
  ELSEIF Fout = 53 THEN
    Fout = 0
  END IF
  CLOSE 1
NEXT i%

LOCATE 7, 1
PRINT SPACE$(19); STRING$(40, "-")
PRINT SPACE$(19); "1 ) Nieuw spel"
IF AantalOpslag% > 0 THEN
  FOR i% = 2 TO AantalOpslag% + 1
    PRINT SPACE$(18); i%; ") Spel"; (i% - 1); "laden"
  NEXT i%
END IF
PRINT SPACE$(19); STRING$(40, "-")
PRINT SPACE$(19); "Toetsen tijdens spelen:"
PRINT SPACE$(19); "s) Spel opslaan"
PRINT SPACE$(19); "esc) Stoppen"
PRINT SPACE$(19); STRING$(40, "-")

DO
  DO
    Keuze$ = INKEY$
  LOOP UNTIL Keuze$ <> ""
  Gekozen% = ASC(Keuze$) - ASC("1")
LOOP UNTIL Gekozen% >= 0 AND Gekozen% <= AantalOpslag%

IF Gekozen% = 0 THEN GOTO Start

HuidigSpel% = Gekozen%

SpelOpslag$ = "OPSLAG" + STR$(HuidigSpel%) + ".SAV"
OPEN SpelOpslag$ FOR INPUT AS 1
LINE INPUT #1, Naam$
LINE INPUT #1, BestandRegel$
Geslacht% = VAL(BestandRegel$)
FOR i% = 0 TO 100
  LINE INPUT #1, BestandRegel$
  Gebeurtenissen(i%) = VAL(BestandRegel$)
NEXT i%
CLOSE 1

GOTO SpelLus

Start:
HuidigSpel% = AantalOpslag% + 1
NaamInvoer:
CLS
PRINT "Hallo avonturier!"
PRINT
INPUT "Wat is je naam"; Naam$
IF LTRIM$(Naam$) = "" THEN
  GOTO NaamInvoer
END IF

SpelLus:
KEY(16) ON
DO
  Overslaan% = 0
  CLS
  CALL ToonGebeurtenis
  CALL ToonActies
LOOP UNTIL Gebeurtenissen(0) <> 0
ERASE Teksten
ERASE Gebeurtenissen

END

Stoppen:
ERASE Teksten
ERASE Gebeurtenissen
COLOR 7
CLS
PRINT "Bedankt voor het spelen!"
END

Foutafhandeling:
  Fout = ERR
  PRINT "ERROR"; Fout
RESUME NEXT

Opslaan:
  SpelOpslag$ = "OPSLAG" + STR$(HuidigSpel%) + ".SAV"
  OPEN SpelOpslag$ FOR OUTPUT AS 1
  PRINT #1, Naam$
  PRINT #1, Geslacht%
  FOR i% = 0 TO 100
    PRINT #1, Gebeurtenissen(i%)
  NEXT i%
  CLOSE 1
RETURN

REM $STATIC
FUNCTION Interpoleer$ (Zin$)
  resultaat$ = ""

  FOR i% = 1 TO LEN(Zin$)
    Karakter$ = MID$(Zin$, i%, 1)
    IF Karakter$ = "$" THEN
      IF MID$(Zin$, i%, 2) = "$n" THEN
        resultaat$ = resultaat$ + Naam$
      END IF
      i% = i% + 1
    ELSEIF Karakter$ = "#" THEN
      Plek% = VAL(MID$(Zin$, i% + 1, 2))
      resultaat$ = resultaat$ + STR$(Gebeurtenissen(Plek%))
      i% = i% + 2
    ELSE
      resultaat$ = resultaat$ + Karakter$
    END IF
  NEXT i%

  Interpoleer$ = resultaat$
END FUNCTION

SUB LeesAvontuur (BestandsNaam$)
  TekstLengte = 0
  Regel = 0

  OPEN BestandsNaam$ FOR INPUT AS #1
  WHILE NOT EOF(1)
    LINE INPUT #1, Element$
    IF Element$ = "END" THEN
      StartActies = TekstLengte + 1
    ELSE
      TekstLengte = TekstLengte + 1
      Teksten(TekstLengte) = Element$
    END IF
  WEND
  PRINT Regel%; TekstLengte%; StartActies%
  CLOSE 1
END SUB

SUB Muteer (Plek%, Bewerking$, Waarde%)
  'PRINT "Muteer"; Plek%; Gebeurtenissen(Plek%); Bewerking$; Waarde%
  SELECT CASE Bewerking$
  CASE "="
    Gebeurtenissen(Plek%) = Waarde%
  CASE "+"
    Gebeurtenissen(Plek%) = Gebeurtenissen(Plek%) + Waarde%
  CASE "-"
    Gebeurtenissen(Plek%) = Gebeurtenissen(Plek%) - Waarde%
  END SELECT
END SUB

SUB Tekst (Kleur%, Regel$)
  COLOR Kleur%
  IF Kleur% <> 4 THEN
    TIMER ON
    FOR i% = 1 TO LEN(Regel$)
      TijdStart = TIMER
      PRINT MID$(Regel$, i%, 1);
      IF Overslaan% = 0 THEN
        DO
          IF INKEY$ = " " THEN
            Overslaan% = -1
          END IF
        LOOP UNTIL TIMER > TijdStart + .03
      END IF
    NEXT i%
    TIMER OFF
    PRINT
  ELSE
    PRINT Regel$
    TIMER ON
    TijdStart = TIMER
    DO
    LOOP UNTIL TIMER > TijdStart + (.03 * LEN(Regel$))
    TIMER OFF
  END IF
  COLOR 7
END SUB

FUNCTION Toegestaan% (Bewering$)
  IF Bewering$ = "END" THEN
    Toegestaan = 0
    EXIT FUNCTION
  END IF

  Toegestaan = 1
  Plek$ = ""
  Bewerking$ = ""
  Waarde$ = ""

  FOR i% = 1 TO LEN(Bewering$)
    Karakter$ = MID$(Bewering$, i%, 1)
    SELECT CASE Karakter$
    CASE "0" TO "9"
      IF Bewerking$ = "" THEN
        Plek$ = Plek$ + Karakter$
      ELSE
        Waarde$ = Waarde$ + Karakter$
      END IF
    CASE "=", "!", ">", "<"
      Bewerking$ = Karakter$
    CASE ";"
      resultaat = Toets(VAL(Plek$), Bewerking$, VAL(Waarde$))
      IF resultaat = 0 THEN
        Toegestaan = 0
        EXIT FUNCTION
      END IF
      Bewerking$ = ""
      Waarde$ = ""
      Plek$ = ""
    END SELECT
  NEXT i%

  IF Bewerking$ <> "" THEN
    Toegestaan = Toets(VAL(Plek$), Bewerking$, VAL(Waarde$))
  END IF
END FUNCTION

FUNCTION Toets (Plek%, Bewerking$, Waarde%)
  'PRINT "Toets"; Plek%, Gebeurtenissen(Plek%), Bewerking$; Waarde%
  IF Bewerking$ = "=" AND Gebeurtenissen(Plek%) = Waarde% THEN
    Toets = 1
  ELSEIF Bewerking$ = "!" AND Gebeurtenissen(Plek%) <> Waarde% THEN
    Toets = 1
  ELSEIF Bewerking$ = ">" AND Gebeurtenissen(Plek%) > Waarde% THEN
    Toets = 1
  ELSEIF Bewerking$ = "<" AND Gebeurtenissen(Plek%) < Waarde% THEN
    Toets = 1
  ELSE
    Toets = 0
  END IF

END FUNCTION

SUB ToonActies
  DIM Actie(10) AS STRING
  DIM ActieTekst(10) AS STRING
  ActieTeller% = 0

  FOR i% = StartActies% TO TekstLengte%
    Bewering$ = Teksten(i%)
    IF Toegestaan%(Bewering$) THEN
      ActieTekst(ActieTeller%) = Teksten(i% + 1)
      Actie(ActieTeller%) = Teksten(i% + 2)
      ActieTeller% = ActieTeller% + 1
    END IF
    i% = i% + 2
  NEXT i%

  COLOR 7
  FOR i% = 1 TO ActieTeller%
    PRINT i%; ") "; ActieTekst(i% - 1)
  NEXT i%

  DO
    DO
      Keuze$ = INKEY$
    LOOP UNTIL Keuze$ <> ""
    Gekozen% = ASC(Keuze$) - ASC("1")
  LOOP UNTIL Gekozen% >= 0 AND Gekozen% < ActieTeller%

  VoerActieUit (Actie(Gekozen%))

END SUB

SUB ToonGebeurtenis
  Verteller% = 2
  FOR i% = 1 TO StartActies% - 1
    Bewering$ = Teksten(i%)
    IF Toegestaan%(Bewering$) THEN
      DO
        i% = i% + 1
        Zin$ = Teksten(i%)
        Bewerking$ = LEFT$(Zin$, 1)
        SELECT CASE Bewerking$
          CASE "&"
            ' Einde en acties
            acties$ = MID$(Zin$, 2, LEN(Zin$) - 1)
            IF acties$ <> "" THEN
              CALL VoerActieUit(acties$)
            END IF
          CASE "*"
            ' Markup instructie
            Commando$ = MID$(Zin$, 2, 1)
            Gegevens$ = MID$(Zin$, 3, LEN(Zin$) - 2)
            SELECT CASE Commando$
              CASE "c"
                Verteller% = VAL(Gegevens$)
              CASE "s"
                SLEEP VAL(Gegevens$)
              CASE "p"
                PLAY Gegevens$
            END SELECT
          CASE ELSE
            CALL Tekst(Verteller%, Interpoleer(Zin$))
        END SELECT
      LOOP UNTIL Bewerking$ = "&"
    ELSE
      DO
        i% = i% + 1
        Zin$ = Teksten(i%)
      LOOP UNTIL LEFT$(Zin$, 1) = "&"
    END IF
  NEXT i%

END SUB

SUB VoerActieUit (Actie$)
  Plek$ = ""
  Bewerking$ = ""
  Waarde$ = ""
  FOR i% = 1 TO LEN(Actie$) + 1
    Karakter$ = MID$(Actie$, i%, 1)
    SELECT CASE Karakter$
      CASE "0" TO "9"
        IF Bewerking$ = "" THEN
          Plek$ = Plek$ + Karakter$
        ELSE
          Waarde$ = Waarde$ + Karakter$
        END IF
      CASE "=", "+", "-"
        Bewerking$ = Karakter$
      CASE ";"
        CALL Muteer(VAL(Plek$), Bewerking$, VAL(Waarde$))
        Bewerking$ = ""
        Waarde$ = ""
        Plek$ = ""
    END SELECT

  NEXT i%

  IF Bewerking$ <> "" THEN
    CALL Muteer(VAL(Plek$), Bewerking$, VAL(Waarde$))
  END IF

END SUB

