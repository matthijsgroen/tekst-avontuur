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

' Gebeurtenissen
' 0 = Spel gewonnen (1), Bezig(0), Verloren(2)
' 1 = Locatie
'   0 = Proloog
'   1 = Bos
'   2 = Akkers
'   3 = Heuvels
'   4 = Moeras
'   5 = Intro
'   6 = Dorp
'   7 = Donker woud
'   8 = Rivier
' 2 = Locatie toestand 0 = look, 1 = Open tas, 2 = leeg
' 4 = Actie van speler
' 5 = Tijdelijke lokatie teller

' 10 = Lokatie status Proloog
' 11 = Lokatie status Mijn
' 12 = Lokatie status Hut
' 13 = Lokatie status Boerderij
' 14 = Lokatie status Molen
' 15 = Lokatie status Smederij
' 16 = Lokatie status Bakkerij
' 17 = Lokatie status Toren

' 20 = beschikbaarheid tas
' 21 = tak
' 22 = geld
' 23 = proloog sleutel / vogelvoer
' 24 = 1 = kapotte houweel, 2 = gerepareerde houweel

' 0. Proloog
1 DATA "1=0;10=0;4=0"
DATA "*c2", "Er was eens, lang geleden een stoere koerier..."
DATA "$Z naam was $n", ""
DATA "$H was altijd super snel in het bezorgen van brieven en pakketjes."
DATA "Dingen bezorgen was dan ook $z lust en $z leven."
DATA "Op het moment dat ons avontuur begint, ligt $n rustig te slapen...", ""
DATA "*s2", "*c14", "<tik, tik, tik>", "*c3", "", "$n: 'huh?'"
DATA "&2=2"

DATA "1=0;10=0;4=1", "*c2"
DATA "Je probeert nog even verder te slapen.", ""
DATA "*c3", "$n: 'Gaaaaap..   ..zzzzzz'", ""
DATA "*s2", "*c14", "<tik, tik, tik>", "*c3", "", "$n: 'hmmmz?'"
DATA "&"

DATA "1=0;10=0;4=2", "*c3"
DATA "$n: 'Ja, ja, ik kom al!'", "", "*c2"
DATA "Met veel tegenzin klim je uit je bed."
DATA "Je strompelt naar de deur."
DATA "", "*c14", "<TIK, TIK, TIK!>", "*c3", ""
DATA "$n: 'Ik ben onderweg!'"
DATA "*c2", "", "Als je de deur van je kamer open doet, staat er niemand.", ""
DATA "*c3", "$n: 'Waar komt dat geluid dan vandaan?'"
DATA "&"

DATA "1=0;10=0;4=3", "*c2"
DATA "Je kijkt de hele kamer rond, op zoek naar de bron van het geluid"
DATA "", "*c3", "$n: 'Hey, er zit een raaf tegen het raam te tikken!'"
DATA "", "*c14", "<TIK, TIK, TIK!>"
DATA "&"

DATA "1=0;10=1;4=1", "*c2", "Je probeert de raaf weg te jagen.", "&"
DATA "1=0;10=1;4=2", "*c2", "Je probeert weer in bed te kruipen.", "&"
DATA "1=0;10=1;4=3", "*c2", "Je probeert het briefje van de raaf te pakken.", "&"
DATA "1=0;10=1;4>0;4<4", "*c2", "", "De raaf pikt naar je hand."
DATA "*c3", " ", "$n: 'Grrr!'", "&"

DATA "1=0;10=0;4=4", "*c2", "Je opent het raam. De raaf springt snel naar binnen."
DATA "Zo te zien heeft hij een brief aan zijn poot hangen."
DATA "&10=1"

DATA "1=0;10=2;2=0;4=4", "*c2", "Je geeft de raaf wat vogelvoer"
DATA "De raaf is je erg dankbaar!"
DATA "&23=4;"

DATA "1=0;10=2;2=0;4=1;23>2", "*c2", "De kast is al open, maar er zit niets interessants meer in", "&"
DATA "1=0;10=2;2=0;4=1;23<2", "*c2", "De kast zit op slot", "&"
DATA "1=0;10=2;2=0;4=1;23=2", "*c2", "Je opent de kast met de sleutel", ""
DATA "*c3", "$n: 'Hey, hier ligt vogelvoer!'"
DATA "", "*c2", "Je pakt het vogelvoer uit de kast", "&4=8;23=3"
DATA "1=0;10=2;2=0;4=2;23=0", "*c2", "Je pakt je tas van achter de deur", "&20=1;22=100;23=1"

DATA "1=0;10=2;2=0;4=0;", "*c2", "Je kijkt de kamer rond", "", "&"
DATA "1=0;10=2;2=0;4=0;20=0", "Je tas hangt aan de achterkant van de deur", "&"
DATA "1=0;10=2;2=0;4=0", "Je hebt een mooie kast tussen je bed en bureau in staan", "&"
DATA "1=0;10=2;2=0;4=0;23!4", "Een redelijk agressieve raaf zit op je bureau", "&"
DATA "1=0;10=2;2=0;4=0;23=4", "Een rustige raaf zit op je bureau", "&"

DATA "1=0;10=2;2=0;4=3;23!4", "*c2", "Je probeert het briefje van de raaf te pakken."
DATA "De raaf pikt naar je hand.", "*c3", "$n: 'Grrr!'"
DATA "&4=8"
DATA "1=0;10=2;2=0;4=3;23=4", "*c2", "Je probeert het briefje van de raaf te pakken."
DATA "De raaf blijft rustig.", "", "Je leest de brief:", "*c6", "*s1", ""
DATA "Beste $n,"
DATA "", "De koning is ernstig ziek."
DATA "Als beste koerier van het land smeken we je om ons te helpen"
DATA "", "Jij bent de enige die snel medicijnen naar ons kan brengen"
DATA "We hopen dat je dit bericht snel ontvangt en je snel de juiste medicijnen kan komen brengen"
DATA "", "Gegroet,", "  De dokter van de koning", "*s2", "*c2", ""
DATA "Je blijft een tijdje naar de brief staren.", "De koning?", "Heeft jouw hulp nodig?"
DATA "Je gaat snel de deur uit, richting een groot avontuur..."
DATA "&10=3;22=3;20=0;2=0"

' 5. Intro
DATA "1=5", "*c2"
DATA "$n wist snel aan de juiste medicijnen te komen,"
DATA "in de vorm van een magische drank."
DATA "$H is nu onderweg naar de koning, om ze te bezorgen."
DATA ""
DATA "*s3", ""
DATA "*c4", "Auw! Een laaghangende tak!", ""
DATA "*c2", "$n viel ineens hard van $z paard...", ""
DATA "*c3", "$n: 'Oh nee! De magische drank is stuk gevallen!'"
DATA "'En mijn paard is er vandoor!'"
DATA "*c2", "", "Daar stond $n dan.. Alleen in het bos, zonder vervoer of medicijnen..."
DATA "&"

' Reizen
' 3 = Reis toestand
'   1 = Arriveer te voet vanuit het noorden
'   2 = Arriveer te voet vanuit het oosten
'   3 = Arriveer te voet vanuit het zuiden
'   4 = Arriveer te voet vanuit het westen
'   5 = Arriveer vanuit sub-lokatie 1
'   6 = Arriveer vanuit sub-lokatie 2
DATA "1=1;3=2", "*c2", "Je wandelt naar het oosten, richting het bos", "", "&3=0"
DATA "1=1;3=4", "*c2", "Je wandelt naar het westen, richting het bos", "", "&3=0"
DATA "1=2;3=2", "*c2", "Je wandelt naar het oosten, richting de akkers", "", "&3=0"
DATA "1=2;3=1", "*c2", "Je wandelt naar het noorden, richting de akkers", "", "&3=0"
DATA "1=3;3=4", "*c2", "Je wandelt naar het westen, richting de heuvels", "", "&3=0"
DATA "1=3;3=1", "*c2", "Je klimt omhoog, richting de heuvels", "", "&3=0"
DATA "1=3;3=5", "*c2", "Je groet de dwerg, en loopt weer richting de weg", "", "&3=0"
DATA "1=4;3=3", "*c2", "Je daalt af naar het zuiden, over een sompig pad, richting het moeras", "", "&3=0"
DATA "1=6;3=2", "*c2", "Je wandelt naar het zuiden, richting het dorp", "", "&3=0"
DATA "1=6;3=4", "*c2", "Je wandelt naar het westen, richting het dorp", "", "&3=0"
DATA "1=6;3=1", "*c2", "Je wandelt naar het noorden, richting het dorp", "", "&3=0"
DATA "1=7;3=2", "*c2", "Je wandelt over een smal kronkelig pad, richting het donkere woud", "", "&3=0"
DATA "1=8;3=3", "*c2", "Je wandelt naar het zuiden, richting de rivier", "", "&3=0"
DATA "1=11;3=5", "*c2", "Je loopt van de weg af, richting de ingang van de mijn.", "", "&3=0"
DATA "1=16;3=5", "*c2", "Uit de bakkerij komt een geur van heerlijke verse broodjes en taarten"
DATA "", "Je stapt watertandend de winkel binnen.", "", "&3=0"
DATA "1=6;3=5;16=1;25=0", "*c2", "Geen idee met wat je aanmoet met de situatie, loop je weer naar buiten", "", "&3=0;1=6"
DATA "1=6;3=5;16=1;25!0", "*c2", "Je loopt weer naar buiten", "", "&3=0;1=6"

' Tas
DATA "2=1;4=1", "*c2", "Je verwijderd de oude heft van de houweel,"
DATA "en gebruikt de stok uit het bos om een nieuwe heft te maken."
DATA "", "Het past! De houweel is weer zo goed als nieuw.", "", "&24=2;21=3"

DATA "2=1;4=2", "*c2", "De koeken ruiken heerlijk,"
DATA "Maar toch besluit je ze voor later te bewaren.", "", "&"

DATA "2=1", "*c15", "Je hebt het volgende bij je:", "&"
DATA "2=1;22>1", "*c7", "-#22 geldstukken", "&"
DATA "2=1;22=1", "*c7", "- 1 geldstuk", "&"
DATA "2=1;21=2", "*c7", "- Een stok, opgeraapt in het bos", "&"
DATA "2=1;23>0;23<3", "*c7", "- De sleutel van je kast", "&23=2"
DATA "2=1;23=3", "*c7", "- Vogelvoer", "&"
DATA "2=1;24=1", "*c7", "- Houweel met gebroken heft", "&"
DATA "2=1;24=2", "*c7", "- Gerepareerde houweel", "&"
DATA "2=1;25=2", "*c7", "- Koeken, vers van de bakker", "&"
DATA "2=1;26=1", "*c7", "- Een fonkelende edelsteen", "&"

' 1. Bos
DATA "1=1;2=0;4=0", "*c2", "Je staat in het bos. Het is een stralende dag."
DATA "De wind laat de blaadjes ritselen.", ""
DATA "In het oosten zijn akkers", "In het westen zijn heuvels", "&"
DATA "1=1;2=0;20=0", "*c2", "Op de grond ligt je tas, en scherven van de fles medicijnen"
DATA "", "*c3", "$n: 'Verdorie, de medicijnen zijn echt verloren'"
DATA "*c2", "", "Je raapt de tas op", "&20=1;22=3"
DATA "1=1;2=0;4=0;21=0", "*c2", "Op de grond ligt een vers afgebroken tak", "&"
DATA "1=1;2=0;4=1", "*c2", "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd."
DATA "Deze tak heeft je best pijn gedaan.", "&21=2"

' 2. Akkers
DATA "1=2;2=0", "*c2"
DATA "Je staat in een boerenlandschap."
DATA "Aan de rechterkant van de weg staat een boerderij."
DATA "Er komt rook achter de boerderij vandaan.", ""
DATA "In het westen ligt een bos"
DATA "In het zuiden loopt een weg richting een dorp", "&"

' 3. Heuvels
DATA "1=3;2=0", "*c2"
DATA "Je staat in de heuvels. De zon schijnt heerlijk."
DATA "Er is een mooi uitzicht van de omgeving.", ""
DATA "Verder op het pad zie je een mijn"
DATA "Iets hoger op de heuvel staat een molen"
DATA "Naar het oosten loopt het pad naar het bos"
DATA "Naar het zuiden loopt een drassig pad een moeras in", "&"

' 4. Moeras
DATA "1=4;2=0", "*c2"
DATA "Je bent aan de rand van het moeras"
DATA "Aan het eind van het pad blijf je staan.", ""
DATA "*c3", "$n: 'Ik ga niet zomaar verder hier heen.'"
DATA " 'Ik wil eerst een goed plan hebben om aan medicijnen te komen'"
DATA " 'Wie weet wat hier allemaal tegen kan komen'"
DATA "*c2", "", "In de verte klinken rare bubbelende geluiden."
DATA "", "Er loopt een pad naar het noorden, omhoog richting heuvels."

DATA "*c3", "$n: 'Ik ga niet zomaar verder hier heen.'"
DATA " 'Ik wil eerst een goed plan hebben om aan medicijnen te komen'"
DATA " 'Wie weet wat hier allemaal tegen kan komen'"
DATA "*c2", "", "In de verte klinken rare bubbelende geluiden."
DATA "", "Er loopt een pad naar het noorden, omhoog richting heuvels.", "&"

' 6. Dorp
DATA "1=6;2=0;4=0", "*c2"
DATA "Je bent in het dorp", ""
DATA "Er hangt een rare stilte", ""
DATA "Aan de linkerkant van de weg zit een bakker"
DATA "Aan de rechterkant van de weg een grote smederij", ""
DATA "De weg loopt verder naar het zuiden, richting een rivier"
DATA "Een klein verborgen pad leid naar het oosten, richting een donker woud", "&"

' 7. Donkere woud
DATA "1=7;2=0", "*c2"
DATA "Je staat aan de rand van een donker woud."
DATA "Het ziet er erg donker en gevaarlijk uit", ""
DATA "*c3", "$n: 'Brr.. hier heb ik niets te zoeken'", "&"

' 8. Rivier
DATA "1=8;2=0", "*c2"
DATA "Je staat bij een ingestorte brug over de rivier"
DATA "Er lijkt geen mogelijkheid te zijn om over te steken"
DATA "De rivier stroomt snel en gevaarlijk", ""
DATA "*c3", "$n: 'Verdorie zo kan ik hier niet weg komen'"
DATA " 'Dit was de weg richting het kasteel van de koning'"
DATA " 'Hier moet ik een oplossing voor gaan vinden'", "&"

' 11. Mijn
DATA "1=11;2=0;11<2;4=0", "*c2"
DATA "Je bent bij de mijn"
DATA "Er ligt een mijnkarretje. Voor de ingang zit een dwerg"
DATA "", "Hij ziet er niet al te vrolijk uit"
DATA "", "*c3", "$n: 'Hallo, kun je mij helpen?'"
DATA "", "*c2", "De dwerg kijkt een beetje boos je aan"
DATA "", "*c9", "Dwerg: 'Grr. Stom ding, stom ding!'"
DATA "&11=1"

DATA "1=11;2=0;11=2;4=0", "*c2"
DATA "Je bent bij de mijn"
DATA "Er ligt een mijnkarretje. Voor de ingang zit Thorin de dwerg"
DATA "", "Hij ziet er niet al te vrolijk uit", ""
DATA "*c9", "Thorin: 'Hallo $n'"
DATA "&"

DATA "1=11;2=0;11=3;4=0", "*c2"
DATA "Je bent bij de mijn"
DATA "Er ligt een mijnkarretje. Thorin de dwerg is in de mijn aan het werk"
DATA "", "Hij ziet er vrolijk uit", ""
DATA "*c9", "Thorin: 'Hallo $n'"
DATA "&"

DATA "1=11;2=0;11=2;4=3", "*c3"
DATA "$n: 'Ik heb je houweel kunnen repareren'"
DATA "", "*c9", "Thorin: 'Echt waar? Laat zien'", ""
DATA "*c3", "$n: 'Alsjeblieft'"
DATA "*c2", "", "Je geeft de gerepareerde houweel aan Thorin. Hij bekijkt hem grondig"
DATA "", "*c9", "Thorin: 'Wow hij is zo goed als nieuw! bedankt!'", "&4=0;24=4"

DATA "1=11;2=0;11=2;4=4", "*c2", "Je bied Thorin de koeken aan", "", "*c3"
DATA "$n: 'Hier, alsjeblieft'"
DATA "", "*c9", "Thorin: 'Whoa die ruiken lekker, bedankt!'", "", "*c2"
DATA "Thorin zit te smullen van de koeken"
DATA "", "*c9", "Thorin: 'Zo daar was ik echt aan toe!'", "&4=0;25=3"

DATA "1=11;2=0;11>1;4=5", "*c3", "$n: 'Thorin, zou jij weten hoe ik aan medicijnen kan komen?'", "", "*c2"
DATA "Thorin denkt even na.", "*c9", "", "Thorin: 'Geen idee, dat zou je het beste in het dorp kunnen vragen'"
DATA " 'Ze hebben daar van alles. Het dorp ligt in het zuid oosten vanaf hier'", "&4=0"

DATA "1=11;2=0;11>1;4=6", "*c3", "$n: 'Thorin, zou jij weten hoe ik aan vervoer kan komen?'", "", "*c2"
DATA "Thorin denkt even na.", "*c9", "", "Thorin: 'Hmm de boer verderop heeft een paard. Je zou kunnen'"
DATA " 'vragen of je die mag lenen?'", "&4=0"

DATA "1=11;2=0;11=2;24=4;25=3", "*c2", "Thorin staat op, en loopt de mijn in. Met nieuwe energie,"
DATA "en een gerepareerde houweel, gaat hij op de edelstenen inhakken", ""
DATA "*c9", "Thorin: 'Hei ho, Hei ho....", "*c2", ""
DATA "Stukken edelsten vallen op de grond. Een stuk rolt naar je toe", "&11=3;4=0"

DATA "1=11;2=0;11=3;4=1;26=0", "*c2"
DATA "Je raapt de edelsteen op. Hij fonkelt mooi in het zonlicht", "&26=1;4=0"

DATA "1=11;2=0;11=2;4=2", "*c3"
DATA "$n: 'Zou ik je houweel mogen hebben?'"
DATA "", "*c9", "Thorin: 'Tuurlijk. Alleen de bovenkant is nog heel.'"
DATA " 'Het heft is gebroken. Ik heb er niets meer aan'", "&4=0;24=1"

DATA "1=11;2=0;11=2;4=1;24!4;25!3", "*c3"
DATA "$n: 'Hoe kan ik je helpen?'"
DATA "", "*c9", "Thorin: 'Heb je iets te eten voor mij?'"
DATA " 'En een nieuwe houweel?'", "&4=0"

DATA "1=11;2=0;11=2;4=1;24=4;25!3", "*c3"
DATA "$n: 'Hoe kan ik je helpen?'"
DATA "", "*c9", "Thorin: 'Heb je iets te eten voor mij?'", "&4=0"

DATA "1=11;2=0;11=2;4=1;24!4;25=3", "*c3"
DATA "$n: 'Hoe kan ik je helpen?'"
DATA "", "*c9", "Thorin: 'Ik heb dringend een nieuwe houweel nodig'", "&4=0"

DATA "1=11;2=0;11=1;4=1", "*c3"
DATA "$n: 'Je bent zelf een stom ding!'"
DATA "", "*c9", "Dwerg: 'Ik heb het niet tegen jou!'", "&"
DATA "1=11;2=0;11=1;4=2", "*c3"
DATA "$n: 'Pardon? Wie is een stom ding?'"
DATA "", "*c9", "Dwerg: 'Sorry, ik heb het niet tegen jou..'", "&"
DATA "1=11;2=0;11=1;4>0;4<3", "*c9"
DATA " 'Sorry van mijn manieren, ik heb wat tegenslag!'"
DATA " 'Mijn naam is Thorin, en wie mag jij wezen?'",""
DATA "*c3", "$n: 'Mijn naam is $n'",""
DATA "*c9", "Thorin: 'Hallo $n. Sorry dat ik zo ruw deed'"
DATA " 'Mijn houweel is gebroken, en ik had net een ader van edelstenen ontdekt!'"
DATA " 'Ik heb zo hard mijn best gedaan om ze los te krijgen'"
DATA " 'Nu is mijn houweel gebroken. En ik heb honger!'"
DATA "*c2", "", "Thorin zucht diep"
DATA "&11=2;4=0"

' 16. Bakkerij
DATA "1=16;16=1;2=0;4=0", "*c2"
DATA "Je staat in de bakkerij. De bakker ziet er verdrietig uit.", ""
DATA "Er hangt een geur van heerlijke verse broodjes en taarten", "&"

DATA "1=16;16=1;4=2;2=0", "*c3"
DATA "$n: 'Hallo? Kan ik iets te eten kopen?'", "", "*c2"
DATA "De bakker lijkt ineens door te hebben dat er iemand in zijn winkel staat"
DATA "", "*c9", "Bakker: 'Oh. Sorry, ik had het even niet in de gaten'"
DATA " 'Ja, we hebben nog wel wat koeken te koop. Ze kosten 2 geldstukken'"
DATA "&4=0;25=1"

DATA "1=16;16=0;4=0", "*c2"
DATA "Je had verwacht op een vriendelijke manier door de bakker begroet te worden,"
DATA "maar helaas. De bakker ziet er verdrietig uit.", "", "*c3", "$n: 'Hallo, mijn naam is $n'"
DATA " 'Ik ben dringend op zoek naar medicijnen voor de koning. Kunt u mij helpen?'"
DATA "", "*c2", "De bakker geeft geen antwoord, en staart voor zich uit.", "&16=1"

DATA "1=16;16=1;2=0;4=1", "*c3"
DATA "$n: 'Hallo? Is alles goed met u?'", "", "&4=0"

DATA "1=16;16=2;2=0;4=0", "*c3", "De bakker kijkt je afwachtend aan.", ""
DATA "*c9", "Bakker: 'De koeken kosten 2 geldstukken'", "&"

DATA "1=16;16=1;2=0;4=3", "*c3", "$n: 'Ik wil graag wat koeken kopen'", "", "*c9"
DATA "Bakker: 'Dat is dan 2 geldstukken'", "&16=2;4=0"

DATA "1=16;16=2;2=0;4=2", "*c3"
DATA "$n: 'Hmm, misschien toch een andere keer'", "", "*c9"
DATA "Bakker: 'Ok, prima'", "&16=1;4=0"

DATA "1=16;16=2;2=0;4=1", "*c3"
DATA "$n: 'Alstublieft, 2 geldstukken'", "", "*c9"
DATA "Bakker: 'Alsjeblieft'", "", "*c2", "De bakker geeft een paar heerlijke koeken"
DATA "&16=1;4=0;25=2;22-2"

DATA "", "*c15", "", "Wat ga je doen:", "&"

DATA "END"

' Acties
2 DATA "20=1;2=0", "Bekijk inhoud van tas", "2=1;4=0"
DATA "1=0;10=0;4<2", "Word wakker", "4=2"
DATA "1=0;10=0;4<2", "Probeer nog even verder te slapen", "4=1"
DATA "1=0;10=0;4=2", "Kijk waar geluid vandaan komt", "4=3"
DATA "1=0;10=0;4=3", "Open raam", "4=4"
DATA "1=0;10=1", "Jaag de raaf weg", "4=1;5+1"
DATA "1=0;10=1", "Het tikken is weg! Probeer weer in bed te kruipen", "4=2;5+1"
DATA "1=0;10=1", "Pak briefje van de raaf", "4=3;5+1"
DATA "1=0;10=1;5>2", "Kijk de kamer rond", "2=0;4=0;10=2"

DATA "1=0;10=2;2!1;4!0", "Kijk de kamer rond", "2=0;4=0"
DATA "1=0;10=2;2=1", "Sluit tas", "2=0;4=0"
DATA "1=0;2=0;10=2", "Open de kast", "4=1"
DATA "1=0;2=0;10=2;20=0", "Pak je tas", "4=2"
DATA "1=0;2=0;10=2", "Pak briefje van de raaf", "4=3"
DATA "1=0;2=0;10=2;23=3", "Geef de raaf te eten", "4=4"
DATA "1=0;10=3", "Begin avontuur", "1=5;4=0"

DATA "1=5", "Bekijk omgeving", "1=1"
DATA "2=1;1!0", "Sluit tas", "2=0;4=0"

' 1. Bos acties
DATA "2=0;1=1;21=0", "Raap de tak op", "4=1"
DATA "2=0;1=1", "Ga naar het oosten, richting de akkers", "1=2;3=2"
DATA "2=0;1=1", "Ga naar het westen, richting de heuvels", "1=3;3=4"

' 2. Akker acties
DATA "2=0;1=2", "Ga naar het westen, richting het bos", "1=1;3=4"
DATA "2=0;1=2", "Ga naar het zuiden, richting het dorp", "1=6;3=3"

' 3. Heuvel acties
DATA "2=0;1=3", "Volg het pad naar de mijn", "1=11;4=0;3=5"
DATA "2=0;1=3", "Ga naar het oosten, richting het bos", "1=1;3=2"
DATA "2=0;1=3", "Ga naar het zuiden, richting het moeras", "1=4;3=3"

' 4. Moeras acties
DATA "2=0;1=4", "Ga naar het noorden, richting de heuvels", "1=3;3=1"

' 6. Dorp Acties
DATA "2=0;1=6", "Wandel naar de bakkerij", "1=16;4=0;3=5"
DATA "2=0;1=6", "Ga naar het noorden, richting de akkers", "1=2;3=1"
DATA "2=0;1=6", "Ga naar het oosten, richting de het donkere woud", "1=7;3=2"
DATA "2=0;1=6", "Ga naar het zuiden, richting de rivier", "1=8;3=3"

' 7. Donker woud acties
DATA "2=0;1=7", "Ga naar het westen, richting het dorp", "1=6;3=4"

' 8. Rivier acties
DATA "2=0;1=8", "Ga naar het noorden, richting het dorp", "1=6;3=1"

' 11. Mijn Acties
DATA "2=0;1=11;11=1;4=0", "Zeg: 'Je bent zelf een stom ding!'", "4=1"
DATA "2=0;1=11;11=1;4=0", "Zeg: 'Pardon?'", "4=2"
DATA "2=0;1=11;11=1;4=0", "Zeg: 'Ok ik ga weer.'", "1=3;3=5"

DATA "2=0;1=11;11=2;4=0", "Zeg: 'Hoe kan ik je helpen?'", "4=1"
DATA "2=0;1=11;11=2;4=0;24=0", "Zeg: 'Zou ik je houweel mogen hebben?'", "4=2"
DATA "2=0;1=11;11=2;4=0;24=2", "Geef gerepareerde houweel aan Thorin", "4=3"
DATA "2=0;1=11;11=2;4=0;25=2", "Geef koeken aan Thorin", "4=4"
DATA "2=0;1=11;11>1;4=0", "Zeg: 'Enig idee hoe ik aan medicijnen kan komen?'", "4=5"
DATA "2=0;1=11;11>1;4=0", "Zeg: 'Enig idee hoe ik aan vervoer kan komen?'", "4=6"
DATA "2=0;1=11;11=2;4=0", "Zeg: 'Sorry, ik moet er weer vandoor'", "1=3;3=5"

DATA "2=0;1=11;11=3;4=0;26=0", "Raap edelsteen op", "4=1"
DATA "2=0;1=11;11=3;4=0", "Zeg: 'Ok, ik ben er weer vandoor'", "1=3;3=5"

' 16. Bakkerij
'DATA "2=0;1=16;16=1;4=0", "Zeg: 'Hallo? Is alles goed met u?'", "4=1"
DATA "2=0;1=16;16=1;4=0;25<1", "Zeg: 'Hallo? Kan ik iets te eten kopen?'", "4=2"
DATA "2=0;1=16;16=1;4=0;25<2;25>0", "Zeg: 'Ik wil graag koeken kopen'", "4=3"
DATA "2=0;1=16;16=1;4=0", "Verlaat de bakkerij", "1=6;3=5;4=0"

' 16.2. Koeken kopen
DATA "2=0;1=16;16=2;4=0;22>1", "Zeg: 'Alstublieft, 2 geldstukken'", "4=1"
DATA "2=0;1=16;16=2;4=0;22>1", "Zeg: 'Hmm, misschien toch andere keer'", "4=2"
DATA "2=0;1=16;16=2;4=0;22<3", "Oei, ik heb niet genoeg geld", "4=2"

' Generale inventory acties
DATA "2=1;24=1;21=2", "Repareer houweel met stok uit bos", "4=1"
DATA "2=1;25=2", "Eet koeken", "4=2"

DATA "END"

DIM SHARED Naam$
DIM SHARED Geslacht%
DIM SHARED Gebeurtenissen(100) AS INTEGER
DIM SHARED HuidigSpel%
DIM SHARED Overslaan%

CLS
LOCATE 5, 20
PRINT "Avontuur!"

AantalOpslag% = 0

FOR i% = 1 TO 5
  SpelOpslag$ = "OPSLAG" + STR$(i%) + ".SAV"
  OPEN SpelOpslag$ FOR INPUT AS 1
  IF Fout = 0 THEN
    AantalOpslag% = AantalOpslag + 1
  END IF
  CLOSE
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
PRINT "Ben je een jongen of een meisje? (j/m)"
DO
  Keuze$ = INKEY$
LOOP UNTIL Keuze$ = "j" OR Keuze$ = "m"

IF Keuze$ = "j" THEN
  Geslacht% = 1
ELSE
  Geslacht% = 0
END IF

SpelLus:
KEY(16) ON
DO
  Overslaan% = 0
  CLS
  CALL ToonGebeurtenis
  CALL ToonActies
LOOP UNTIL Gebeurtenissen(0) <> 0

END

Stoppen:
COLOR 7
CLS
PRINT "Bedankt voor het spelen!"
END

Foutafhandeling:
  Fout = ERR
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

FUNCTION Interpoleer$ (Zin$)
  Resultaat$ = ""

  FOR i% = 1 TO LEN(Zin$)
    Karakter$ = MID$(Zin$, i%, 1)
    IF Karakter$ = "$" THEN
      SELECT CASE MID$(Zin$, i%, 2)
      CASE "$n"
        Resultaat$ = Resultaat$ + Naam$
      CASE "$h"
        IF Geslacht% THEN
          Resultaat$ = Resultaat$ + "hij"
        ELSE
          Resultaat$ = Resultaat$ + "zij"
        END IF
      CASE "$H"
        IF Geslacht% THEN
          Resultaat$ = Resultaat$ + "Hij"
        ELSE
          Resultaat$ = Resultaat$ + "Zij"
        END IF
      CASE "$z"
        IF Geslacht% THEN
          Resultaat$ = Resultaat$ + "zijn"
        ELSE
          Resultaat$ = Resultaat$ + "haar"
        END IF
      CASE "$Z"
        IF Geslacht% THEN
          Resultaat$ = Resultaat$ + "Zijn"
        ELSE
          Resultaat$ = Resultaat$ + "Haar"
        END IF
      END SELECT
      i% = i% + 1
    ELSEIF Karakter$ = "#" THEN
      Plek% = VAL(MID$(Zin$, i% + 1, 2))
      Resultaat$ = Resultaat$ + STR$(Gebeurtenissen(Plek%))
      i% = i% + 2
    ELSE
      Resultaat$ = Resultaat$ + Karakter$
    END IF
  NEXT i%

  Interpoleer$ = Resultaat$
END FUNCTION

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

  RESTORE 2
  DO
    READ Bewering$
    IF Toegestaan%(Bewering$) THEN
      READ ActieTekst(ActieTeller%), Actie(ActieTeller%)
      ActieTeller% = ActieTeller% + 1
    ELSEIF Bewering$ <> "END" THEN
      ' Negeer data tot volgende bewering
      READ Zin$, Aanpassing$
    END IF
  LOOP UNTIL Bewering$ = "END"

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
  RESTORE 1
  Verteller% = 2
  DO
    READ Bewering$
    IF Toegestaan%(Bewering$) THEN
      DO
        READ Zin$
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
    ELSEIF Bewering$ <> "END" THEN
      DO
        READ Zin$
      LOOP UNTIL LEFT$(Zin$, 1) = "&"
    END IF
  LOOP UNTIL Bewering$ = "END"

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

