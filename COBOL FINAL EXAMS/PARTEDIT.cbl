       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTEDIT.
       AUTHOR. POMELA DOMINGUEZ.
      *
      **********     Final Project    ******************************
      *   PARTEDIT Subroutine is called to validate the PARTS
      *   section of Transaction Records as per the following
      *   specifications:
      *
      *   The following fields are required.
      *   a)  Part-Number
      *   b)  Part-Name
      *   c)  Vehicle-Make
      *   d)  Vehicle-Model
      *   e)  Vehicle-Year
      *
      *   Vehicle Make must either of the following values:
      *   'CHR, FOR, GM, VW, TOY, JAG, PEU, BMW'
      *   Vehicle Year must be between 1990 and 2019
      *   Weeks Lead Time must be numeric and between 1 & 4.
      *
      *   Subroutine returns the following:
      *   a)  Total fields validation errors in ERROR-CTR
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
       77  INVALID-PART-DESC           PIC X(30) VALUE SPACES.
       LINKAGE SECTION.
       COPY PARTS.
       77  VALID-RECORD                PIC 9 VALUE ZERO.
       77  ERROR-CTR                   PIC 9.
       77  EXCP-DESCRIPTION            PIC X(50).
      *
       PROCEDURE DIVISION USING PARTS-REC, VALID-RECORD, ERROR-CTR,
                                EXCP-DESCRIPTION.
       MAIN-RTN.
           PERFORM VALIDATE-RECORD-PART-FIELDS.

           IF ERROR-CTR > 0
              MOVE 8 TO VALID-RECORD
           ELSE
              MOVE 0 TO VALID-RECORD
           END-IF.
      *
           GOBACK.
      *
       VALIDATE-RECORD-PART-FIELDS.
      *
           MOVE SPACES TO EXCP-DESCRIPTION.
      *
           IF PART-NUMBER  EQUAL ZEROES OR
              PART-NUMBER  EQUAL SPACES
              MOVE 'PART # '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR.
      *
           IF PART-NAME    EQUAL ZEROES OR
              PART-NAME    EQUAL SPACES
              MOVE 'PART NAME '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR.
      *
           IF NOT VALID-VEHICLE-MAKE
              MOVE 'VEHICLE MAKE '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR.
      *
           IF VEHICLE-MODEL EQUAL ZEROES OR
              VEHICLE-MODEL EQUAL SPACES
              MOVE 'VEHICLE MODEL '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR.
      *
           IF FUNCTION NUMVAL(VEHICLE-YEAR) < 1990 AND
              FUNCTION NUMVAL(VEHICLE-YEAR) > 2019
              MOVE 'VEHICLE YEAR '   TO INVALID-PART-DESC
              PERFORM PREPARE-EXCP-DESCRIPTION
                     ADD 1 TO ERROR-CTR.
      *
           IF WEEKS-LEAD-TIME < 1 or
              WEEKS-LEAD-TIME > 4
              MOVE 'WEEKS LEADTIME '   TO INVALID-PART-DESC
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