       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDREDIT.
       AUTHOR. NANCY SLOCUM.
      *
      **********     Final Project    ******************************
      *   LAST EDITED: 09/30/2020 AT 12:28PM PDT
      *   ADDREDIT Subroutine is called to validate the ADDRESES
      *   section of Transaction Records as per the following
      *   specifications:
      *
      *   The following fields are required.
      *   a)  Valid Address Type
      *   b)  At least one(1) Address Line
      *   c)  City
      *   d)  State
      *   e)  Zip Code
      *
      *   Address must contain a valid State/Zip Code combination
      *   as defined by the STATEZIP file.
      *
      *   Subroutine returns the following:
      *   a)  Total field validation errors in ERROR-CTR
      *   b)  A short description of the error in validation.
      *
      *   PREPARE-EXCP-DESCRIPTION paragraph, prepares, concatenates
      *   all probable short description errors identified during
      *   field validations.
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Invalid field strings
       01  INVALID-ADDR-DESC          PIC X(30) VALUE SPACES.
           88  TYPE-STR        VALUE 'ADDRESS TYPE '.
           88  ADDR1-STR       VALUE 'ADDRESS 1 '.
           88  CITY-STR        VALUE 'CITY '.
           88  STATE-STR       VALUE 'STATE '.
           88  ZIP-STR         VALUE 'ZIP CODE '.
      *Table search variables & flags
       77  TABLE-MAX                   PIC 9(2) VALUE 72.
       01  STATE-TO-SEARCH             PIC X(2).
       01  VALID-ZIP                   PIC X(1).
           88  ZIP-IN-RANGE          VALUE 'Y'.
       LINKAGE SECTION.
       01  DATA-RECEIVED.
           05  VLDT-ADDR-TYPE       PIC X(01).
               88  VALID-ADDR-TYPE  VALUES '1', '2', '3'.
           05  VLDT-ADDR1           PIC X(15).
               88  INV-ADDR1        VALUES SPACES, LOW-VALUE, ALL '0'.
           05  FILLER               PIC X(30).
           05  VLDT-CITY            PIC X(15).
               88  INV-CITY         VALUES  SPACES, LOW-VALUE, ALL '0'.
           05  VLDT-STATE           PIC X(02).
               88  INV-STATE        VALUES  SPACES, LOW-VALUE, ALL '0'.
           05  VLDT-ZIPCODE         PIC 9(05).
               88  INV-ZIP          VALUES ZEROES.
           05  FILLER                  PIC X(05).
       77  VALID-RECORD                PIC 9.
       77  ERROR-CTR                   PIC 9.
       77  EXCP-DESCRIPTION            PIC X(50).
      *  Table for state/zip search
       01 STATE-ZIP-TABLE.
           05 STATE-ZIP-RANGE OCCURS 72 TIMES
              ASCENDING KEY IS WS-STATE-ID INDEXED BY ZIP-IDX .
             10  FILLER                PIC X(16).
             10  WS-STATE-ID           PIC X(2).
             10  WS-LOW-ZIP            PIC 9(5).
             10  WS-HIGH-ZIP           PIC 9(5).

       PROCEDURE DIVISION USING DATA-RECEIVED, VALID-RECORD, ERROR-CTR,
                                EXCP-DESCRIPTION, STATE-ZIP-TABLE.
       MAIN-RTN.

             DISPLAY 'AT THE START OF ADDRESS EDIT '.
             IF  INV-ADDR1
                  DISPLAY 'INV-ADDR1 VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-ADDR1 VALUE IS NOT TRUE'
             END-IF.
             IF  INV-CITY
                  DISPLAY 'INV-CITY  VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-CITY  VALUE IS NOT TRUE'
             END-IF.
             IF  INV-STATE
                  DISPLAY 'INV-STATE VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-STATE VALUE IS NOT TRUE'
             END-IF.
             IF  INV-ZIP
                  DISPLAY 'INV-ZIP   VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-ZIP   VALUE IS NOT TRUE'
             END-IF.
             DISPLAY 'VALID RECORD ' VALID-RECORD.
             DISPLAY 'ERROR CTR ' ERROR-CTR.
             DISPLAY 'EXCEPTION DESCRIPTION ' EXCP-DESCRIPTION.
             DISPLAY  '  '


           INITIALIZE INVALID-ADDR-DESC.
           MOVE SPACES TO EXCP-DESCRIPTION.
           PERFORM NUMERIC-CHECKS.
           PERFORM VALIDATE-RECORD-ADDR-FIELDS.
           PERFORM VALIDATE-STATE-AND-ZIP.
      *  Determines whether PO will be printed in the main report
           IF ERROR-CTR > 0
              MOVE 8 TO VALID-RECORD
           ELSE
              MOVE 0 TO VALID-RECORD
           END-IF.

             DISPLAY 'AT THE END OF ADDREDIT '.
             IF  INV-ADDR1
                  DISPLAY 'INV-ADDR1 VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-ADDR1 VALUE IS NOT TRUE'
             END-IF.
             IF  INV-CITY
                  DISPLAY 'INV-CITY  VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-CITY  VALUE IS NOT TRUE'
             END-IF.
             IF  INV-STATE
                  DISPLAY 'INV-STATE VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-STATE VALUE IS NOT TRUE'
             END-IF.
             IF  INV-ZIP
                  DISPLAY 'INV-ZIP   VALUE IS TRUE '
             ELSE
                  DISPLAY 'INV-ZIP   VALUE IS NOT TRUE'
             END-IF.
             DISPLAY ' VALID PO ' VALID-RECORD.
             DISPLAY 'ERROR CTR ' ERROR-CTR.
             DISPLAY 'EXCEPTION DESCRIPTION ' EXCP-DESCRIPTION.
             DISPLAY  '  '


           GOBACK.
      *
       NUMERIC-CHECKS.
      *  Verifies & "corrects" field formats.
      *  These checks don't "count" as errors, because if the field is
      *  in the wrong format, it will create a SOC7 when we try to test
      *  it for valid data. Instead we set the field to zero or spaces
      *  and allow the error to be caught during validation checks.
           IF VLDT-ADDR-TYPE IS NOT NUMERIC
              MOVE ZERO TO VLDT-ADDR-TYPE
           END-IF.
           IF VLDT-ZIPCODE IS NOT NUMERIC
              MOVE ZEROES TO VLDT-ZIPCODE
           END-IF.
      *
       VALIDATE-RECORD-ADDR-FIELDS.
      *  Test for valid address type
           IF NOT VALID-ADDR-TYPE
               SET TYPE-STR TO TRUE
               PERFORM PREPARE-EXCP-DESCRIPTION
           END-IF.
      *  Test for valid address 1 line
           IF INV-ADDR1
              SET ADDR1-STR TO TRUE
              PERFORM PREPARE-EXCP-DESCRIPTION
           END-IF.
      *  Test city
           IF INV-CITY
              SET CITY-STR TO TRUE
              PERFORM PREPARE-EXCP-DESCRIPTION
           END-IF.

       VALIDATE-STATE-AND-ZIP.
      *  First, test for blanks/invalid data (don't search for blanks).
           IF INV-STATE OR INV-ZIP
              NEXT SENTENCE
           ELSE
              PERFORM STATE-ZIP-TEST
           END-IF.
           IF INV-STATE
              SET STATE-STR TO TRUE
              PERFORM PREPARE-EXCP-DESCRIPTION
           END-IF.
           IF INV-ZIP
              SET ZIP-STR TO TRUE
              PERFORM PREPARE-EXCP-DESCRIPTION
           END-IF.
      *
       STATE-ZIP-TEST.
      *  Begin the state-zip correlation
           MOVE VLDT-STATE TO STATE-TO-SEARCH.
           SET ZIP-IDX TO 1
           SEARCH STATE-ZIP-RANGE VARYING ZIP-IDX
      *    If you get to the end without finding anything, error out
              AT END
                     SET ZIP-STR TO TRUE
                     PERFORM PREPARE-EXCP-DESCRIPTION
              WHEN WS-STATE-ID(ZIP-IDX) = STATE-TO-SEARCH AND
                   WS-LOW-ZIP(ZIP-IDX)  <= VLDT-ZIPCODE   AND
                   WS-HIGH-ZIP(ZIP-IDX) >= VLDT-ZIPCODE
                           SET ZIP-IN-RANGE TO TRUE
                           DISPLAY 'ZIP OK' VLDT-ZIPCODE
           END-SEARCH.
      *    don't know why you're not working above.
      *   Let's try perform.
      **     MOVE VLDT-STATE TO STATE-TO-SEARCH.
      **     MOVE 'N' TO VALID-ZIP.
      **     SET ZIP-IDX TO 1.
      **     PERFORM VARYING ZIP-IDX FROM 1 BY 1 UNTIL
      **             ZIP-IDX > 72 OR ZIP-IN-RANGE
      **
      **       IF STATE-TO-SEARCH = WS-STATE-ID(ZIP-IDX)  AND
      **            VLDT-ZIPCODE >= WS-LOW-ZIP(ZIP-IDX)   AND
      **            VLDT-ZIPCODE <= WS-HIGH-ZIP(ZIP-IDX)
      **                 SET ZIP-IN-RANGE TO TRUE
      **                 DISPLAY 'ZIP OK' VLDT-ZIPCODE
      **       END-IF
      **     END-PERFORM.
      **     IF  ZIP-IN-RANGE
      **         NEXT SENTENCE
      **     ELSE
      **         SET ZIP-STR TO TRUE
      **         PERFORM PREPARE-EXCP-DESCRIPTION
      **     END-IF.
       PREPARE-EXCP-DESCRIPTION.
           IF  EXCP-DESCRIPTION EQUAL SPACES
               STRING 'INVALID ' DELIMITED BY SIZE
               INVALID-ADDR-DESC DELIMITED BY '  '
               INTO EXCP-DESCRIPTION
           ELSE
               STRING  EXCP-DESCRIPTION DELIMITED BY '  '
               '/ '    DELIMITED BY SIZE
               INVALID-ADDR-DESC DELIMITED BY '  '
               INTO EXCP-DESCRIPTION
           END-IF.
           ADD 1 TO ERROR-CTR.
           DISPLAY EXCP-DESCRIPTION.