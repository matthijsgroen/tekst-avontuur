DECLARE SUB wisActies ()
DECLARE FUNCTION welkeActie% ()
DECLARE SUB tekst (Kleur%, Regel$)
DECLARE SUB start ()
DECLARE SUB forest ()

KEY 15, CHR$(0) + CHR$(1)
KEY(15) ON
ON KEY(15) GOSUB Stoppen

DIM SHARED Plaats$
DIM SHARED Naam$
DIM SHARED Hij$
DIM SHARED Zijn$
DIM SHARED acties(8) AS STRING
DIM SHARED actieTeksten(8) AS STRING

Plaats$ = "start"

SELECT CASE Plaats$
CASE "start"
  CALL start
CASE "forest"
  CALL forest
END SELECT

END
Stoppen:
END

SUB forest
PRINT "Je bent in het bos"

END SUB

SUB start
  CLS
  INPUT "Hoi, wat is jouw naam"; Naam$
  INPUT "Ben je een jongen of een meisje? (j/m) ", gender$
  IF UCASE$(LEFT$(gender$, 1)) = "J" THEN
    Hij$ = "hij"
    Zijn$ = "zijn"
  ELSE
    Hij$ = "zij"
    Zijn$ = "haar"
  END IF

  CLS
  'CALL tekst(2, "Er was eens, lang geleden een stoere koerier.")
  'CALL tekst(2, Hij$ + " was altijd super snel in het bezorgen van brieven en pakketjes.")
  'CALL tekst(2, "Dingen bezorgen was dan ook " + Zijn$ + " lust en " + Zijn$ + " leven.")
  'PRINT
  'CALL tekst(2, "Op een dag was " + Hij$ + " onderweg met een heel belangrijke pakket voor de koning...")
  'CALL tekst(2, "De koning was ernstig ziek, en had dringend medicijnen nodig!")
  'PRINT
  'CALL tekst(2, "Gelukkig is " + Naam$ + " een magisch drankje aan het bezorgen!")

  'SLEEP 3
  'PRINT
  'CALL tekst(4, "Auw! een laag hangende tak!")
  'PRINT

  'CALL tekst(2, Naam$ + " viel ineens hard van " + Zijn$ + " paard...")
  'CALL tekst(2, "Oh nee! De magische drank is stuk gevallen!")
  'CALL tekst(2, "Bovendien, is je paard er vandoor!")
  'PRINT
  'CALL tekst(2, "Daar stond " + Naam$ + " dan.. Alleen in het bos, zonder vervoer of medicijnen...")

  Gekeken = 0

  DO
    CALL wisActies
    i% = 0
    acties(i%) = "Rondkijken"
    actieTeksten$(i%) = "Rondkijken"
    IF Gekeken THEN
      i% = i% + 1
      acties$(i%) = "Oosten"
      actieTeksten$(i%) = "Ga naar het oosten"
      i% = i% + 1
      acties$(i%) = "Westen"
      actieTeksten$(i%) = "Ga naar het westen"
    END IF

    Opdracht$ = acties$(welkeActie%)

    SELECT CASE Opdracht$
    CASE "Rondkijken"
      CLS
      CALL tekst(2, "Je staat in het bos. Het is een stralende ochtend")
      PRINT
      CALL tekst(2, "In het oosten zijn akkers")
      CALL tekst(2, "In het westen zijn heuvels")
      Gekeken = -1
    CASE "Oosten"
      CLS
      PRINT "We wissen je geheugen!"
      Gekeken = 0
    END SELECT

  LOOP UNTIL Plaats$ <> "Start"

END SUB

SUB tekst (Kleur%, Regel$)
  COLOR Kleur%
  IF Kleur% = 2 THEN
    TIMER ON
    Overslaan = 0
    FOR i% = 1 TO LEN(Regel$)
      TijdStart = TIMER
      PRINT MID$(Regel$, i%, 1);
      IF Overslaan = 0 THEN
        DO
          IF INKEY$ = " " THEN
            Overslaan = -1
          END IF
        LOOP UNTIL TIMER > TijdStart + .05
      END IF
    NEXT i%
    TIMER OFF
    PRINT
  ELSE
    PRINT Regel$
    TIMER ON
    TijdStart = TIMER
    DO
    LOOP UNTIL TIMER > TijdStart + (.05 * LEN(Regel$))
    TIMER OFF
  END IF
  COLOR 7
END SUB

FUNCTION welkeActie%
  PRINT
  PRINT "Wat ga je doen?"

  maxActies% = 0
  FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    IF acties$(i%) <> "" THEN
      PRINT (i% + 1); ") "; acties$(i%)
      maxActies% = maxActies% + 1
    END IF
  NEXT i%

  DO
    DO
      Keuze$ = INKEY$
    LOOP UNTIL Keuze$ <> ""
    Gekozen% = ASC(Keuze$) - ASC("1")
  LOOP UNTIL Gekozen% >= 0 AND Gekozen% < maxActies%

  welkeActie = Gekozen%

END FUNCTION

SUB wisActies
  FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    acties$(i%) = ""
    actieTeksten$(i%) = ""
  NEXT i%
END SUB

