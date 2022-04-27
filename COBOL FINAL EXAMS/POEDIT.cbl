       IDENTIFICATION DIVISION.
       PROGRAM-ID. POEDIT.
       AUTHOR. NANCY SLOCUM.
      *
      **********    Final Project   *********************************
      *   LAST EDITED: 09/30/2020 AT 11:31AM PDT
      *   POREDIT Subroutine is called to validate the PURCHASE
      *   section of Transaction Records as per the following
      *   specifications:
      *
      *   The following fields are required.
      *   a)  PO Number
      *   b)  Buyer Code
      *   c)  Order Date
      *   d)  Quantity
      *
      *  CRITERIA:
      *   - Quantity must be between 0 and 999,999
      *   - If Quantity is 0, Unit-Price must also be 0
      *   - If > 0, Unit Price must be between $1 and $1,000,000.00
      *   - Order date must be a valid date
      *   - Delivery date is optional but if it exists, it must be a
      *     valid date later than the order date.
      *
      *  Dates validated with CEEDAYS via the CHECK-DATE paragraph.
      *
      *   Subroutine returns the following:
      *   a)  Total field validation errors in ERROR-CTR
      *   b)  A short description of the error in validation.
      *
      *   PREPARE-EXCP-DESC paragraph, prepares, concatenates
      *   all probable short description errors identified during
      *   field validations.
      *
      ***************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Invalid field strings
         01  INVALID-PO-DESC          PIC X(30) VALUE SPACES.
             88 PO-STR          VALUE 'PO NUMBER '.
             88 BUYER-STR       VALUE 'BUYER CODE '.
             88 QUANTITY-STR    VALUE 'QUANTITY '.
             88 PRICE-STR       VALUE 'UNIT PRICE '.
             88 ORDER-STR       VALUE 'ORDER DATE '.
             88 DELIVERY-STR    VALUE 'DELIVERY DATE '.
      *Validation Flags & Switches
         01  QUANTITY-AND-PRICE-FLAGS.
             05  FILLER               PIC 9(1) VALUE 0.
                 88  Q-IS-0                VALUE 1.
             05  FILLER               PIC 9(1) VALUE 0.
                 88  P-TOO-SMALL          VALUE 1.
             05  FILLER               PIC 9(1) VALUE 0.
                 88  P-TOO-BIG            VALUE 1.
             05  FILLER               PIC 9(1) VALUE 0.
                 88  P-IS-0               VALUE 1.
      *Date checking
         01  DATE-TO-CHECK              PIC 9(8).
         01  INV-DATE-FLAG              PIC X.
             88 INV-DATE                VALUE 'Y'.
         01  WS-PO-ACT-DATE             PIC 9(9) COMP.
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
      *
       LINKAGE SECTION.
         01  DATA-RECEIVED.
             05  VLDT-PO                    PIC X(6).
                 88 INV-PO         VALUES   SPACES, LOW-VALUE, ALL '0'.
             05  VLDT-BUYER                 PIC X(3).
                 88 INV-BUYER      VALUES   SPACES, LOW-VALUE, ALL '0'.
             05  VLDT-QUANTITY              PIC S9(7) COMP.
             05  VLDT-PRICE                 PIC S9(7)V99 COMP-3.
             05  VLDT-ORDER                 PIC 9(8).
                 88 INV-ORDER      VALUE    ZERO.
             05  VLDT-DELIVERY              PIC 9(8).
                 88 INV-DELIVERY   VALUE    ZERO.
         77  VALID-PO                       PIC 9.
         77  ERROR-CTR                      PIC 9.
         77  EXCP-DESC                      PIC X(50).
      *
       PROCEDURE DIVISION USING DATA-RECEIVED, VALID-PO, ERROR-CTR,
                                 EXCP-DESC.
       MAIN-RTN.
             INITIALIZE INVALID-PO-DESC, QUANTITY-AND-PRICE-FLAGS.
             MOVE SPACES TO EXCP-DESC.
             PERFORM NUMERIC-CHECKS.
             PERFORM PO-AND-BUYER-VALIDATION.
             PERFORM QUANTITY-AND-PRICE-VALIDATION.
             PERFORM DATE-VALIDATION.
      * Determines whether PO will be printed in the main report
           IF ERROR-CTR > 0
               MOVE 8 TO VALID-PO
           ELSE
               MOVE 0 TO VALID-PO
           END-IF.
      *
           GOBACK.
       NUMERIC-CHECKS.
      *  Verifies & "corrects" field formats.
      *  These checks don't "count" as errors, because if the field is
      *  in the wrong format, it will create a SOC7 when we try to test
      *  it for valid data. Instead we set the field to zero or spaces
      *  and allow the error to be caught during validation checks.
      *  Ensure alphanum is alphanum
           IF VLDT-BUYER IS NUMERIC
              MOVE SPACES TO VLDT-BUYER
           END-IF.
      *  Ensure numeric is numeric
           IF VLDT-PRICE IS NOT NUMERIC
                MOVE ZEROES TO VLDT-PRICE
                PERFORM PRICE-ERROR
      *  Note: Price is only sometimes an error when zero, this counts
           END-IF.
           IF VLDT-ORDER IS NOT NUMERIC
                MOVE ZEROES TO VLDT-ORDER
           END-IF.
           IF VLDT-DELIVERY IS NOT NUMERIC
                MOVE ZEROES TO VLDT-DELIVERY
           END-IF.
      *
       PO-AND-BUYER-VALIDATION.
      *  Test PO Number
           IF INV-PO
                SET PO-STR TO TRUE
                PERFORM PREPARE-EXCP-DESC
           END-IF.
      *  Test Buyer Code
            IF INV-BUYER
                 SET BUYER-STR TO TRUE
                 PERFORM PREPARE-EXCP-DESC
            END-IF.
       QUANTITY-AND-PRICE-VALIDATION.
      *  Set Flags for Quantity & Unit Price tests
            IF VLDT-QUANTITY >= 1000000
               PERFORM QUANTITY-ERROR
               CONTINUE
      *       ^ Don't test price if quantity is bad
            ELSE
            IF VLDT-PRICE = 0
               IF VLDT-QUANTITY > 0
                  PERFORM PRICE-ERROR
               END-IF
            ELSE
               IF VLDT-PRICE > 100000000*> Implied decimal adds 2 0s
                   PERFORM PRICE-ERROR
               END-IF
            END-IF.
       QUANTITY-ERROR.
             SET QUANTITY-STR TO TRUE.
             PERFORM PREPARE-EXCP-DESC.
       PRICE-ERROR.
             SET PRICE-STR TO TRUE.
             PERFORM PREPARE-EXCP-DESC.
      *  Test order date
       DATE-VALIDATION.
            IF INV-ORDER
                CONTINUE
            ELSE
                 MOVE VLDT-ORDER TO DATE-TO-CHECK
                 PERFORM CHECK-DATE
                     IF INV-DATE
                        CONTINUE
                     ELSE
                        NEXT SENTENCE
                     END-IF
            END-IF
            SET ORDER-STR TO TRUE
            PERFORM PREPARE-EXCP-DESC.
      *  Check whether delivery date exists & if it does, test it
            IF VLDT-DELIVERY IS NOT ZERO
               PERFORM VALIDATE-DELIVERY
            END-IF.
       VALIDATE-DELIVERY.
            IF VLDT-DELIVERY > VLDT-ORDER
               MOVE VLDT-DELIVERY TO DATE-TO-CHECK
               PERFORM CHECK-DATE
                   IF INV-DATE
                      CONTINUE
                   ELSE
                      NEXT SENTENCE
                   END-IF
            END-IF
            SET DELIVERY-STR TO TRUE
            PERFORM PREPARE-EXCP-DESC.
      *  Write string to exception description
       PREPARE-EXCP-DESC.
            IF  EXCP-DESC EQUAL SPACES
                STRING       'INVALID ' DELIMITED BY SIZE
                INVALID-PO-DESC DELIMITED BY '  '
                INTO EXCP-DESC
            ELSE
                STRING  EXCP-DESC DELIMITED BY '  '
                '/ '    DELIMITED BY SIZE
                INVALID-PO-DESC DELIMITED BY '  '
                INTO EXCP-DESC
            END-IF.
            ADD 1 TO ERROR-CTR
            DISPLAY EXCP-DESC.
      *  Validate a given date string using CEEDAYS
       CHECK-DATE.
           MOVE SPACE TO INV-DATE-FLAG.
           MOVE  DATE-TO-CHECK TO  WS-DATE-IN-STR-CEE.
            DISPLAY ' DATE IN  ' WS-DATE-IN-CEE
            CALL 'CEEDAYS' USING WS-DATE-IN-CEE, WS-PICSTR-IN,
                                 WS-PO-ACT-DATE, FC.
            IF FC-SEV NOT = ZERO
               DISPLAY ' BAD DATE ' WS-PO-ACT-DATE
               SET INV-DATE TO TRUE
            ELSE
               DISPLAY ' GOOD DATE '  WS-PO-ACT-DATE
            END-IF.