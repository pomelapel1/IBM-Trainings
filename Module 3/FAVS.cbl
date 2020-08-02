       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
      ***** Workshop 3.1a - Pomela Dominguez
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FAV-REC.
           05  ARTIST-NAME              PIC X(30).
           05  NUMBER-OF-MUSICIANS      PIC 9(02).
           05  MUSICAL-GENRE            PIC X(12).
           05  COST.
                10 CD-COST              PIC 9(3)V99.
                10 SHIPPING-COST        PIC 9(2)V99.
                10 TAX                  PIC 9(2)V99.
           05  BAND-IS-STILL-TOGETHER   PIC X(1).
       01  COST-OUT.
           05  CD-COST-OUT              PIC $$,$$$.99.
           05  SHIPPING-COST-OUT        PIC $$,$$$.99.
           05  TAX-OUT                  PIC $$,$$$.99.
       77  TOTAL-CD-COST                PIC 9(4)V99.
       77  TOTAL-CD-COST-OUT            PIC $$,$$$.99.
       PROCEDURE DIVISION.
           MOVE "JOURNEY"               TO ARTIST-NAME.
           MOVE 5                       TO NUMBER-OF-MUSICIANS.
           MOVE "ROCK BAND"             TO MUSICAL-GENRE.
           MOVE 35                      TO CD-COST.
           MOVE 'Y'                     TO BAND-IS-STILL-TOGETHER.
           MOVE 15.99                   TO SHIPPING-COST.
           IF  CD-COST > 40
               COMPUTE TAX = (CD-COST * 0.06)
           ELSE
               COMPUTE TAX = (CD-COST * 0.10).
           COMPUTE TOTAL-CD-COST = CD-COST + SHIPPING-COST + TAX.
           MOVE TOTAL-CD-COST           TO TOTAL-CD-COST-OUT.
           MOVE ZEROES                  TO COST-OUT.
      *    MOVE COST                    TO COST-OUT.
           MOVE CD-COST                 TO CD-COST-OUT.
           MOVE SHIPPING-COST           TO SHIPPING-COST-OUT.
           MOVE TAX                     TO TAX-OUT.
           DISPLAY "Artist              : " ARTIST-NAME.
           DISPLAY "# of musicians      : " NUMBER-OF-MUSICIANS.
           DISPLAY "Musical Genre       : " MUSICAL-GENRE.
           DISPLAY "Band still together ? " BAND-IS-STILL-TOGETHER.
           DISPLAY "Cost of CD          : " CD-COST-OUT.
           DISPLAY "SHIPPING COST       : " SHIPPING-COST-OUT.
           DISPLAY "TAXES               : " TAX-OUT.
           DISPLAY "                        -------".
           DISPLAY "Total CD Price      : " TOTAL-CD-COST-OUT.
           GOBACK.
