       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
      *
      ********** Workshop 8.1   -   Pomela Dominguez    ************
      *
      *   To create an RFP report:
      *   a)  One / per RFP / Musician - showing the calculated
      *       cost with all commercial factors applied
      *       (Instrument base-price, New/Used, Tax at 8%, shipping
      *        etc).
      *
      *       Layout the report as you see fit.
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVRFP-FILE     ASSIGN TO RFPIN.
           SELECT FAVRFP-REPORT   ASSIGN TO FAVINRPT.
       DATA DIVISION.
       FILE SECTION.
       FD  FAVRFP-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFP-REC.
       01  RFP-REC.
           05  ARTIST-ACCT-NO                 PIC X(08).
           05  ARTIST-MUSICAL-GENRE           PIC X(06).
               88 ROCK            VALUE 'ROCK'.
               88 JAZZ            VALUE 'JAZZ'.
               88 FUSION          VALUE 'FUSION'.
           05  MUSICIAN.
               10  MUSICIAN-LNAME             PIC X(15).
               10  MUSICIAN-FNAME             PIC X(15).
           05  MUSICIAN-INSTRUMENT-TYPE       PIC X(06).
               88  KEYBOARD       VALUE  'KEYS'.
               88  VOCALS         VALUE  'VOCALS'.
               88  GUITAR         VALUE  'GUITAR'.
               88  BASS           VALUE  'BASS'.
               88  DRUMS          VALUE  'DRUMS'.
               88  PERCUSSION     VALUE  'PERC'.
           05  INSTRUMENT-QUALITY             PIC X(01).
               88  USED-FLAG      VALUE  'U'.
               88  NEW-FLAG       VALUE  'N'.
               88  PREMIUM-FLAG   VALUE  'P'.
           05  MAX-MUSICIAN-BUDGET-AMOUNT     PIC 9(5)V99.
           05  SHIP-TO                        PIC X(3).
               88  IN-COUNTRY     VALUE  "IN".
               88  OUT-OF-COUNTRY VALUE  "OUT".
           05  FILLER                         PIC X(19).
       FD  FAVRFP-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 200 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PROP-REC.
       01  PROP-REC.
           05  ARTIST-ACCT-NO-O               PIC X(08).
           05  FILLER                         PIC X(2).
           05  ARTIST-MUSICAL-GENRE-O         PIC X(06).
               88  ROCK           VALUE 'ROCK'.
               88  JAZZ           VALUE 'JAZZ'.
               88  FUSION         VALUE 'FUSION'.
           05  FILLER                         PIC X(2).
           05  MUSICIAN-O.
               10  MUSICIAN-LNAME-O           PIC X(15).
               10  FILLER                     PIC X(2) VALUE ', '.
               10  MUSICIAN-FNAME-O           PIC X(15).
           05  FILLER                         PIC X(2).
           05  MUSICIAN-INSTRUMENT-TYPE-O     PIC X(10).
               88  KEYBOARD-O       VALUE  'KEYS'.
               88  VOCALS-O         VALUE  'VOCALS'.
               88  GUITAR-O         VALUE  'GUITAR'.
               88  BASS-O           VALUE  'BASS'.
               88  DRUMS-O          VALUE  'DRUMS'.
               88  PERCUSSION-O     VALUE  'PERC'.
           05  FILLER                         PIC X(2).
           05  INSTRUMENT-QUALITY-O           PIC X(7).
           05  FILLER                         PIC X(2).
           05  SHIP-TO-O                      PIC X(6).
           05  FILLER                         PIC X(2).
           05  MAX-BUDGET-O                   PIC $$$,$$$.99.
           05  FILLER                         PIC X(2).
           05  COST-PER-INSTRUMENT-O          PIC $$,$$$,$$$.99CR.
           05  FILLER                         PIC X(2).
           05  ADDITIONAL-COSTS-O.
               10  SHIPPING-COST-O            PIC $$,$$$.99CR.
               10  FILLER                     PIC X(2).
               10  TAX-O                      PIC $,$$$.99CR.
               10 FILLER                      PIC X(2).
           05  TOTAL-INSTR-COST               PIC $$,$$$,$$$.99CR.
           05  FILLER                         PIC X(5).
           05  RFP-NOTE                       PIC X(15).
      *
      *
       WORKING-STORAGE SECTION.
       01  HEADER-LINE-1.
           05  FILLER                   PIC X(6)  VALUE 'DATE: '.
           05  WS-CURR-YEAROUT          PIC 9(4).
           05  FILLER                   PIC X     VALUE '/'.
           05  WS-CURR-MONTHOUT         PIC 9(2).
           05  FILLER                   PIC X     VALUE '/'.
           05  WS-CURR-DAYOUT           PIC 9(2).
           05  FILLER                   PIC X(50) VALUE SPACES.
           05  FILLER                   PIC X(24) VALUE
                                        '  REQUEST FOR PROPOSAL  '.
           05  FILLER                   PIC X(45) VALUE SPACES.
       01  HEADER-LINE-2.
           05  FILLER                   PIC X(1)  VALUE SPACE.
           05  FILLER                   PIC X(6)  VALUE 'ACCT #'.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(5)  VALUE 'GENRE'.
           05  FILLER                   PIC X(14) VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE 'MUSICIAN'.
           05  FILLER                   PIC X(14) VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE 'INSTRUMENT'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE 'QUALITY'.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE 'SHIP TO'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE '  BUDGET'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(15)  VALUE
                                                 'COST/INSTRUMENT'.
           05  FILLER                   PIC X(3)  VALUE SPACE.
           05  FILLER                   PIC X(8)  VALUE 'SHIPPING'.
           05  FILLER                   PIC X(5)  VALUE SPACES.
           05  FILLER                   PIC X(5)  VALUE 'TAXES'.
           05  FILLER                   PIC X(10)  VALUE SPACES.
           05  FILLER                   PIC X(5)  VALUE 'TOTAL'.
      *
       01  HEADER-LINE-3.
      *
           05  FILLER                   PIC X(1)  VALUE SPACE.
           05  FILLER                   PIC X(6)  VALUE '======'.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(5)  VALUE '====='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(32) VALUE
                                   '================================'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(10) VALUE '=========='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE '======='.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE '======='.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE '========'.
           05  FILLER                   PIC X(2)  VALUE SPACES.
           05  FILLER                   PIC X(15)  VALUE
                                                 '==============='.
           05  FILLER                   PIC X(3)  VALUE SPACE.
           05  FILLER                   PIC X(8)  VALUE '========'.
           05  FILLER                   PIC X(3)  VALUE SPACES.
           05  FILLER                   PIC X(8)  VALUE '========'.
           05  FILLER                   PIC X(7)  VALUE SPACES.
           05  FILLER                   PIC X(10)  VALUE '=========='.
      *
       01  EXCEPTION-LINE.
           05  FILLER                   PIC X(18) VALUE SPACES.
           05  FILLER                   PIC X(35) VALUE
           '** Note:  Please contact our store '.
           05  FILLER                   PIC X(40) VALUE
           'to look for items that suits the budget.'.
      *
       01  FOOTER-LINE-1.
           05 FILLER                    PIC X(36) VALUE
                              "Number of Artists in this Report: ".
           05 TOT-ARTIST-OUT            PIC ZZ9.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-2.
           05 FILLER                    PIC X(36) VALUE
                              "Gross Sales Revenue             : ".
           05 GRAND-TOTAL-COST-OUT      PIC $$$,$$9.99.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-3.
           05 FILLER                    PIC X(36) VALUE
                              "Average CD Sales Cost           : ".
           05 AVG-CD-COST-OUT           PIC $$$,$$9.99.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-4.
           05 FILLER                    PIC X(36) VALUE
                              "Lowest CD Cost                  : ".
           05 LOWEST-COST-OUT           PIC $$$,$$9.99.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  FOOTER-LINE-5.
           05 FILLER                    PIC X(36) VALUE
                              "Highest CD Cost                 : ".
           05 HIGHEST-COST-OUT          PIC $$$,$$9.99.
           05 FILLER                    PIC X(50) VALUE SPACES.
      *
       01  ACC-CTRS-CONSTANTS.
           05  INST-PRICING.
               10  KEYS-COST            PIC 9(5)V99 VALUE  3017.89.
               10  VOCALS-COST          PIC 9(5)V99 VALUE  599.05.
               10  GUITAR-COST          PIC 9(5)V99 VALUE  2648.99.
               10  BASS-COST            PIC 9(5)V99 VALUE  1876.10.
               10  DRUM-COST            PIC 9(5)V99 VALUE  3087.22.
               10  PERC-COST            PIC 9(5)V99 VALUE  799.99.
      *
           05  INST-BASE-PRICE          PIC S9(5)V99 VALUE ZEROES.
           05  INST-ADJ-PRICE           PIC S9(7)V99 VALUE ZEROES.
           05  INST-TAX                 PIC S9(3)V99 VALUE ZEROES.
           05  INST-SHIP-COST           PIC S9(4)V99 VALUE ZEROES.
           05  TOT-INST-COST            PIC S9(7)V99 VALUE ZEROES.
      *
           05  TOT-COST                 PIC 9(4)V99 VALUE ZEROES.
           05  TOT-ARTIST               PIC 9(3)    VALUE ZEROES.
           05  GRAND-TOTAL-COST         PIC 9(5)V99 VALUE ZEROES.
           05  AVG-CD-COST              PIC 9(4)V99 VALUE ZEROES.
           05  HIGHEST-COST             PIC 9(3)V99 VALUE ZEROES.
           05  LOWEST-COST              PIC 9(3)V99 VALUE ZEROES.
     *
       01  EOF-STAT                     PIC X       VALUE SPACE.
           88  END-OF-FILE              VALUE 'Y'.
      *
       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR          PIC 9(4).
           05  WS-CURRENT-MONTH         PIC 9(2).
           05  WS-CURRENT-DAY           PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-RTN.
           PERFORM OPEN-FILES.
           MOVE   'N' TO EOF-STAT.
           READ    FAVRFP-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
           PERFORM DISPLAY-HEADINGS.
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
      *    COMPUTE AVG-CD-COST = (GRAND-TOTAL-COST / TOT-ARTIST).
           PERFORM DISPLAY-FOOTERS.
           PERFORM CLOSE-FILES.
           GOBACK.
      *
       OPEN-FILES.
           OPEN INPUT  FAVRFP-FILE.
           OPEN OUTPUT FAVRFP-REPORT.
      *
       PROCESS-RECORDS.
           MOVE ARTIST-ACCT-NO           TO ARTIST-ACCT-NO-O.
           MOVE ARTIST-MUSICAL-GENRE     TO ARTIST-MUSICAL-GENRE-O.
           MOVE MUSICIAN-LNAME           TO MUSICIAN-LNAME-O.
           MOVE MUSICIAN-FNAME           TO MUSICIAN-FNAME-O.
           MOVE MUSICIAN-INSTRUMENT-TYPE TO MUSICIAN-INSTRUMENT-TYPE-O.
           MOVE MAX-MUSICIAN-BUDGET-AMOUNT TO MAX-BUDGET-O.
      *
           PERFORM PROCESS-PRICE-INSTRUMENT.
      *
           MOVE INST-ADJ-PRICE           TO COST-PER-INSTRUMENT-O.
           MOVE INST-SHIP-COST           TO SHIPPING-COST-O.
           MOVE INST-TAX                 TO TAX-O.
           MOVE TOT-INST-COST            TO TOTAL-INSTR-COST.
      *
      *
           IF    TOT-INST-COST  > MAX-MUSICIAN-BUDGET-AMOUNT
                 MOVE  'See note **.' to RFP-NOTE
           END-IF.
      *
           WRITE PROP-REC            AFTER ADVANCING 2 LINES.
           MOVE  SPACES              TO    PROP-REC.
           READ    FAVRFP-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
       PROCESS-PRICE-INSTRUMENT.
      *
           EVALUATE TRUE
               WHEN KEYBOARD    MOVE KEYS-COST   TO INST-BASE-PRICE
               WHEN VOCALS      MOVE VOCALS-COST TO INST-BASE-PRICE
               WHEN GUITAR      MOVE GUITAR-COST TO INST-BASE-PRICE
               WHEN BASS        MOVE BASS-COST   TO INST-BASE-PRICE
               WHEN DRUMS       MOVE DRUM-COST   TO INST-BASE-PRICE
               WHEN PERCUSSION  MOVE PERC-COST   TO INST-BASE-PRICE
               WHEN OTHER       MOVE ZEROES      TO INST-BASE-PRICE
           END-EVALUATE.
      *
           EVALUATE TRUE
               WHEN USED-FLAG     IN INSTRUMENT-QUALITY
                    COMPUTE INST-ADJ-PRICE =
                           (INST-BASE-PRICE - (INST-BASE-PRICE * .20))
                    MOVE  'USED'  TO INSTRUMENT-QUALITY-O
               WHEN NEW-FLAG      IN INSTRUMENT-QUALITY
                    COMPUTE INST-ADJ-PRICE =
                           (INST-BASE-PRICE * 1)
                    MOVE  'NEW'   TO INSTRUMENT-QUALITY-O
               WHEN PREMIUM-FLAG  IN INSTRUMENT-QUALITY
                    COMPUTE INST-ADJ-PRICE =
                           (INST-BASE-PRICE * 1.20)
                    MOVE  'PREMIUM' TO INSTRUMENT-QUALITY-O
           END-EVALUATE.
      *
           EVALUATE TRUE
               WHEN IN-COUNTRY    IN SHIP-TO
                    COMPUTE INST-SHIP-COST =
                           (INST-ADJ-PRICE * .10)
                    MOVE 'LOCAL'  TO SHIP-TO-O
               WHEN OUT-OF-COUNTRY IN SHIP-TO
                    COMPUTE INST-SHIP-COST =
                           (INST-ADJ-PRICE * .20)
                    MOVE 'ABROAD' TO SHIP-TO-O
           END-EVALUATE.
      *
           COMPUTE INST-TAX = (INST-ADJ-PRICE * .08).
           COMPUTE TOT-INST-COST = (INST-ADJ-PRICE +
                         INST-SHIP-COST + INST-TAX).
      *
      *
       DISPLAY-HEADINGS.
           MOVE  FUNCTION  CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE  WS-CURRENT-YEAR        TO WS-CURR-YEAROUT.
           MOVE  WS-CURRENT-MONTH       TO WS-CURR-MONTHOUT.
           MOVE  WS-CURRENT-DAY         TO WS-CURR-DAYOUT.
      *
           MOVE  SPACES     TO     PROP-REC.
           WRITE PROP-REC.
           WRITE PROP-REC.
           MOVE  SPACES     TO     PROP-REC.
           WRITE PROP-REC   FROM   HEADER-LINE-1
                            AFTER ADVANCING 3 LINES.
           MOVE  SPACES     TO     PROP-REC.
           WRITE PROP-REC.
           WRITE PROP-REC.
           WRITE PROP-REC   FROM   HEADER-LINE-2
                            AFTER ADVANCING 3 LINES.

           MOVE  SPACES     TO     PROP-REC.
           WRITE PROP-REC   FROM   HEADER-LINE-3
                            AFTER ADVANCING 1 LINE.
           MOVE  SPACES     TO     PROP-REC.
           WRITE PROP-REC.
      *    WRITE PROP-REC  FROM   HEADER-LINE-4.
      *    MOVE  SPACES     TO     PROP-REC.
      *    WRITE PROP-REC.
      *
       DISPLAY-FOOTERS.
           MOVE  SPACES       TO     PROP-REC.
           WRITE PROP-REC.
           WRITE PROP-REC.
           WRITE PROP-REC  FROM EXCEPTION-LINE.
      *
       CLOSE-FILES.
           CLOSE FAVRFP-FILE.
           CLOSE FAVRFP-REPORT.