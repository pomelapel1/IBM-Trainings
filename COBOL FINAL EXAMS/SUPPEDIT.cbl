       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPEDIT.
       AUTHOR. POMELA DOMINGUEZ.
      *
      ********** Workshop 24  -   Pomela Dominguez    ************
      *
      *   Final Project.
      *
      *
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  INVALID-PART-DESC                PIC X(30) VALUE SPACES.
       01  WS-SUPPLIER-ACT-DATE             PIC 9(9) COMP.
       01  WS-PICSTR-IN.
           05  WS-PICSTR-LTH-IN             PIC S9(4) COMP VALUE 8.
           05  WS-PICSTR-STR-IN             PIC X(8) VALUE 'YYYYMMDD'.
       01  WS-DATE-IN-CEE.
           05  WS-DATE-IN-LTH-CEE           PIC S9(4) COMP VALUE 8.
           05  WS-DATE-IN-STR-CEE           PIC X(8).
       01  FC.
           05  FC-SEV                       PIC S9(4) COMP.
           05  FC-MSG                       PIC S9(4) COMP.
           05  FC-CTW                       PIC X.
           05  FC-FAC                       PIC X(3).
           05  FC-ISI                       PIC S9(8) COMP.
       LINKAGE SECTION.
       COPY SUPPLIER.
       77  VALID-SUPPLIER                  PIC 9.
       77  ERROR-CTR                       PIC 9.
       77  EXCP-DESCRIPTION                PIC X(50).
      *
       PROCEDURE DIVISION USING SUPPLIER-REC, VALID-SUPPLIER, ERROR-CTR,
                                EXCP-DESCRIPTION.
       MAIN-RTN.
           PERFORM VALIDATE-SUPPLIER-FIELDS.

           IF ERROR-CTR > 0
              MOVE 8 TO VALID-SUPPLIER
           ELSE
              MOVE 0 TO VALID-SUPPLIER
           END-IF.
      *
           GOBACK.
      *
       VALIDATE-SUPPLIER-FIELDS.
      *
           MOVE SPACES TO EXCP-DESCRIPTION.
      *
           IF SUPPLIER-CODE IS EQUAL ZEROES  OR
              SUPPLIER-CODE IS EQUAL TO SPACES
              MOVE ' SUPPLIER CODE '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR.
      *
           IF NOT VALID-SUPPLIER-TYPES
              MOVE ' SUPPLIER TYPE '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR
           ELSE
              IF SUPPLIER-TYPE = 'S' AND
                 SUPPLIER-RATING NOT = '3'
                 MOVE ' RATING FOR SUB-CONTRACTOR '
                        TO INVALID-PART-DESC
                 PERFORM PREPARE-EXCP-DESCRIPTION
                 ADD 1 TO ERROR-CTR
              END-IF
           END-IF.
      *
           IF NOT VALID-SUPPLIER-RATING
              MOVE ' SUPPLIER RATING '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR.
      *
           IF NOT VALID-SUPPLIER-STATUS
              MOVE ' SUPPLIER STATUS '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR.
      *
           IF SUPPLIER-NAME IS NUMERIC OR
              SUPPLIER-NAME IS EQUAL TO SPACES
              MOVE ' SUPPLIER NAME '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR.
      *
      *    RESEARCH FIRST HOW TO USE CEEDAYS.
      *
      *     INITIALIZE WS-SUPPLIER-ACT-DATE, WS-PICSTR-IN
      *                WS-DATE-IN-CEE, FC.
           MOVE  SUPPLIER-ACT-DATE TO WS-DATE-IN-STR-CEE,
                                           WS-SUPPLIER-ACT-DATE.
           CALL 'CEEDAYS' USING WS-DATE-IN-CEE, WS-PICSTR-IN,
                                WS-SUPPLIER-ACT-DATE, FC.
           IF FC-SEV NOT = ZERO
      *        DISPLAY 'BAD DATE'
              MOVE ' SUPPLIER ACTION DATE '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR
      *     ELSE
      *        DISPLAY 'GOOD DATE'
           END-IF.
      *     DISPLAY WS-SUPPLIER-ACT-DATE.
      *
           IF SUPPLIER-PERF IS EQUAL TO ZERO
              MOVE ' SUPPLIER PERF '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
              ADD 1 TO ERROR-CTR.
      *
       PREPARE-EXCP-DESCRIPTION.
           IF  EXCP-DESCRIPTION EQUAL SPACES
               STRING       'INVALID ' DELIMITED BY SIZE
               INVALID-PART-DESC DELIMITED BY '  '
               INTO EXCP-DESCRIPTION
           ELSE
               STRING  EXCP-DESCRIPTION DELIMITED BY '  '
               '/ '    DELIMITED BY SIZE
               INVALID-PART-DESC DELIMITED BY '  '
               INTO EXCP-DESCRIPTION
           END-IF.
      *     DISPLAY EXCP-DESCRIPTION.