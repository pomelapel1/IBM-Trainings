       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
      ***** Workshop 7.3        - Pomela Dominguez
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN-FILE    ASSIGN TO FAVIN.
           SELECT FAVIN-REPORT  ASSIGN TO FAVINRPT.
       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-REC.
       01  FAVIN-REC.
           05  ARTIST-NAME              PIC X(30).
           05  NO-OF-MUSICIANS          PIC 9(02).
           05  MUSICAL-GENRE            PIC X(12).
           05  COST.
                10 CD-COST              PIC 9(3)V99.
                10 SHIPPING-COST        PIC 9(2)V99.
                10 TAX                  PIC 9(2)V99.
           05  BAND-STILL-TOGETHER      PIC X(1).
       FD  FAVIN-REPORT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-RPT.
       01  FAVIN-RPT.
           05  ARTIST-NAME-OUT          PIC X(30).
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  NO-OF-MUSICIANS-OUT      PIC 9(02).
           05  FILLER                   PIC X(6) VALUE SPACES.
           05  MUSICAL-GENRE-OUT        PIC X(12).
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  COST-OUT.
                10  CD-COST-OUT         PIC $$$$.99.
                10  FILLER              PIC X(3) VALUE SPACES.
                10  SHIPPING-COST-OUT   PIC $$$.99.
                10  FILLER              PIC X(2) VALUE SPACES.
                10  TAX-OUT             PIC $$$.99.
                10  FILLER              PIC X(3) VALUE SPACES.
                10  TOT-COST-OUT        PIC $$,$$$.99.
                10  FILLER              PIC X(5) VALUE SPACES.
           05  BAND-STILL-TOGETHER-OUT  PIC X(1).
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
           05  FILLER                   PIC X(25) VALUE SPACES.
           05  FILLER                   PIC X(24) VALUE
                                        'FAVORITE ARTISTS REPORT'.
           05  FILLER                   PIC X(45) VALUE SPACES.
       01  HEADER-LINE-2.
           05  FILLER                   PIC X(12) VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE 'ARTISTS'.
           05  FILLER                   PIC X(14) VALUE SPACES.
           05  FILLER                   PIC X(6)  VALUE ' # OF '.
           05  FILLER                   PIC X(7)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE 'MUSICAL'.
           05  FILLER                   PIC X(9) VALUE SPACES.
           05  FILLER                   PIC X(2)  VALUE 'CD'.
           05  FILLER                   PIC X(6)  VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'SHIP'.
           05  FILLER                   PIC X(4)  VALUE SPACES.
           05  FILLER                   PIC X(3)  VALUE 'TAX'.
           05  FILLER                   PIC X(7)  VALUE SPACES.
           05  FILLER                   PIC X(7)  VALUE 'TOTAL'.
           05  FILLER                   PIC X(5)  VALUE SPACES.
      *
       01  HEADER-LINE-3.
           05  FILLER                   PIC X(13) VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'NAME'.
           05  FILLER                   PIC X(15) VALUE SPACES.
           05  FILLER                   PIC X(9)  VALUE 'MUSICIANS'.
           05  FILLER                   PIC X(6)  VALUE SPACES.
           05  FILLER                   PIC X(6)  VALUE 'GENRE'.
           05  FILLER                   PIC X(8) VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'COST'.
           05  FILLER                   PIC X(5)  VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'COST'.
           05  FILLER                   PIC X(4)  VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'COST'.
           05  FILLER                   PIC X(7)  VALUE SPACES.
           05  FILLER                   PIC X(4)  VALUE 'COST'.
           05  FILLER                   PIC X(5)  VALUE SPACES.
      *
       01  HEADER-LINE-4.
           05  FILLER                   PIC X(30) VALUE
                               "=============================".
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  FILLER                   PIC X(9) VALUE "=========".
           05  FILLER                   PIC X(2) VALUE SPACES.
           05  FILLER                   PIC X(12) VALUE "============".
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  FILLER                   PIC X(7) VALUE "=======".
           05  FILLER                   PIC X(4) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "======".
           05  FILLER                   PIC X(3) VALUE SPACES.
           05  FILLER                   PIC X(6) VALUE "======".
           05  FILLER                   PIC X(3) VALUE SPACES.
           05  FILLER                   PIC X(8) VALUE "========".
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
       01  ACC-CTRS.
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
           READ    FAVIN-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
      *     Set Initial Highest-Cost and Lowest-Cost
      *     to compare CD cost for every record read.
      *
           ADD     CD-COST, SHIPPING-COST, TAX TO HIGHEST-COST.
           ADD     CD-COST, SHIPPING-COST, TAX TO LOWEST-COST.
      *
           PERFORM DISPLAY-HEADINGS.
           PERFORM PROCESS-RECORDS UNTIL END-OF-FILE.
           COMPUTE AVG-CD-COST = (GRAND-TOTAL-COST / TOT-ARTIST).
           PERFORM DISPLAY-FOOTERS.
           PERFORM CLOSE-FILES.
           GOBACK.
      *
       OPEN-FILES.
           OPEN INPUT  FAVIN-FILE.
           OPEN OUTPUT FAVIN-REPORT.
      *
       PROCESS-RECORDS.
           MOVE ARTIST-NAME         TO  ARTIST-NAME-OUT.
           MOVE NO-OF-MUSICIANS     TO  NO-OF-MUSICIANS-OUT.
           MOVE MUSICAL-GENRE       TO  MUSICAL-GENRE-OUT.
           MOVE CD-COST             TO  CD-COST-OUT.
           MOVE SHIPPING-COST       TO  SHIPPING-COST-OUT.
           MOVE TAX                 TO  TAX-OUT.
           COMPUTE TOT-COST = (CD-COST + SHIPPING-COST + TAX).
      *
           MOVE TOT-COST            TO TOT-COST-OUT.
           ADD  TOT-COST            TO GRAND-TOTAL-COST.
           ADD  1                   TO TOT-ARTIST.
           IF   TOT-COST > HIGHEST-COST
                MOVE TOT-COST        TO HIGHEST-COST
           END-IF.
           IF   TOT-COST < LOWEST-COST
                MOVE TOT-COST        TO LOWEST-COST
           END-IF.
      *
           WRITE FAVIN-RPT.
           READ    FAVIN-FILE
                   AT END MOVE 'Y' TO EOF-STAT.
      *
       DISPLAY-HEADINGS.
           MOVE  FUNCTION  CURRENT-DATE TO WS-CURRENT-DATE.
           MOVE  WS-CURRENT-YEAR        TO WS-CURR-YEAROUT.
           MOVE  WS-CURRENT-MONTH       TO WS-CURR-MONTHOUT.
           MOVE  WS-CURRENT-DAY         TO WS-CURR-DAYOUT.
      *
           MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT.
           WRITE FAVIN-RPT.
      *    MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT  FROM   HEADER-LINE-1.
           MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT.
           WRITE FAVIN-RPT.
           WRITE FAVIN-RPT  FROM   HEADER-LINE-2.
           MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT  FROM   HEADER-LINE-3.
           MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT.
           WRITE FAVIN-RPT  FROM   HEADER-LINE-4.
           MOVE  SPACES     TO     FAVIN-RPT.
           WRITE FAVIN-RPT.
      *
       DISPLAY-FOOTERS.
           MOVE  SPACES       TO     FAVIN-RPT.
           WRITE FAVIN-RPT.
           WRITE FAVIN-RPT.
           MOVE  TOT-ARTIST   TO TOT-ARTIST-OUT.
           MOVE  AVG-CD-COST  TO AVG-CD-COST-OUT.
           MOVE  GRAND-TOTAL-COST TO GRAND-TOTAL-COST-OUT.
           MOVE  LOWEST-COST  TO LOWEST-COST-OUT.
           MOVE  HIGHEST-COST TO HIGHEST-COST-OUT.
           WRITE FAVIN-RPT    FROM  FOOTER-LINE-1.
           WRITE FAVIN-RPT    FROM  FOOTER-LINE-2.
           WRITE FAVIN-RPT    FROM  FOOTER-LINE-3.
           WRITE FAVIN-RPT    FROM  FOOTER-LINE-4.
           WRITE FAVIN-RPT    FROM  FOOTER-LINE-5.
      *
       CLOSE-FILES.
           CLOSE  FAVIN-FILE.
           CLOSE FAVIN-REPORT.