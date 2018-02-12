       IDENTIFICATION DIVISION.
       PROGRAM-ID.     CBLBJC05.
       AUTHOR.         BRADEN CRIM
       DATE-WRITTEN.   1/20/2017
      ******************************************************************
      *THIS PROGRAM CALCULATES THE TOTAL COST OF A RENTAL INCLUDING ALL
      *UTILITIES, DISCOUNTS, AND PREMIUM RENTALS. IT WILL FLAG ANY 
      *RENTAL OVER $1000 WITH THREE *** AT THE END OF THE DETAIL LINE.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RENT-ORDER
               ASSIGN TO 'C:\COBOL\MONBILLS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RENT-SALES
               ASSIGN TO 'C:\COBOL\RENT.PRT'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD  RENT-ORDER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 24 CHARACTERS.
       01  I-RENT-REC.
           05  I-BLD-CODE              PIC XX.
               88  R7YTPP              VALUE 'R7','YT','PP'.
               88  BPCT                VALUE 'BP','CT'.
               88  IAJK                VALUE 'IA','JK'.
           05  I-UNIT                  PIC 99.
               88  UNIT1-8             VALUE 1 THRU 8.
               88  UNIT9-16            VALUE 9 THRU 16.
               88  UNIT17-25           VALUE 17 THRU 25.
               88  UNIT2325            VALUE 23,25.
           05  I-TENANTS               PIC 9.
           05  I-ELECTRIC              PIC 999V99.
           05  I-GAS                   PIC 999V99.
           05  I-WATER                 PIC 999V99.
           05  I-GARBAGE               PIC 99V99.

       FD  RENT-SALES
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.
       01  PRTLINE                     PIC X(132).
       WORKING-STORAGE SECTION.
       01  MISC.
           05  EOF                     PIC XXX          VALUE 'NO'.
           05  CURRENT-DATE-AND-TIME.
               10  CURRENT-YEAR            PIC XXXX.
               10  CURRENT-MONTH           PIC XX.
                   88 JULY-DECEMBER        VALUE '07','12'.
               10  CURRENT-DAY             PIC XX.
               10  CURRENT-TIME            PIC X(11).
           05  C-PCTR                      PIC 99       VALUE ZERO.
           05  C-BASE-RATE                 PIC 9999V99.
           05  C-DISCOUNT                  PIC 999V99.
           05  C-PREMIUM                   PIC 999V99.
           05  C-TENANT-CHARGE             PIC 999V99.
           05  C-SUBTOTAL                  PIC 9999V99.
           05  C-UTILITY                   PIC 9999V99.
           05  C-TOTAL-RENT                PIC 9999V99.
           05  C-DISC-PREM                 PIC S9999V99.
           05  C-DISC-CTR                  PIC 999      VALUE ZERO.
           05  C-PREM-CTR                  PIC 999      VALUE ZERO.
           05  C-GT-BASE-RENT              PIC 9(5)V99  VALUE ZERO.
           05  C-GT-TENANT                 PIC 9(5)V99  VALUE ZERO.
           05  C-GT-DISC-PREM              PIC S9(6)V99 VALUE ZERO.
           05  C-GT-SUBTOTAL               PIC 9(6)V99  VALUE ZERO.
           05  C-GT-UTILTIES               PIC 9(6)V99  VALUE ZERO.
           05  C-GT-RENT                   PIC 9(6)V99  VALUE ZERO.
       01  TITLE-LINE.
           05  FILLER                  PIC X(6)    VALUE 'DATE: '.
           05  H1-DATE.
               10  H1-MONTH            PIC 99.
               10  FILLER              PIC X       VALUE '/'.
               10  H1-DAY              PIC 99.
               10  FILLER              PIC X       VALUE '/'.
               10  H1-YEAR             PIC 9999.
           05  FILLER                  PIC X(42)   VALUE SPACES.
           05  FILLER                  PIC X(15)
                   VALUE "FURLY'S RENTALS".
           05  FILLER                  PIC X(51)   VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR                  PIC 99.
       01  REPORT-LINE.
           05  FILLER                  PIC X(8)    VALUE 'COBBJC05'.
           05  FILLER                  PIC X(45)   VALUE SPACES.
           05  FILLER                  PIC X(22)
                   VALUE 'BILLABLE RENT - BRADEN'.
       01  HEADING1.
           05  FILLER                  PIC X(23)   VALUE SPACES.
           05  FILLER                  PIC X(4)    VALUE 'BASE'.
           05  FILLER                  PIC X(2)    VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'TENANT'.
           05  FILLER                  PIC X(2)    VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'TENANT'.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'PREMIUM/'.
           05  FILLER                  PIC X(53)   VALUE SPACES.
           05  FILLER                  PIC X(5)    VALUE 'TOTAL'.
       01  HEADING2.
           05  FILLER                  PIC X(15)
                   VALUE 'RENTAL BUILDING'.
           05  FILLER                  PIC X(5)    VALUE ' UNIT'.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  FILLER                  PIC XXXX    VALUE 'RENT'.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'NUMBER'.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  FILLER                  PIC X(6)    VALUE 'CHARGE'.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'DISCOUNT'.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'SUBTOTAL'.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'ELECTRIC'.
           05  FILLER                  PIC XXXX    VALUE SPACES.
           05  FILLER                  PIC XXX     VALUE 'GAS'.
           05  FILLER                  PIC XXXX    VALUE SPACES.
           05  FILLER                  PIC X(5)    VALUE 'WATER'.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  FILLER                  PIC X(7)    VALUE 'GARBAGE'.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  FILLER                  PIC X(9)    VALUE 'UTILITIES'.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  FILLER                  PIC X(8)    VALUE 'RENT DUE'.
       01  DETAIL-LINE.
           05  O-RENTAL-BUILDING       PIC X(15).
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-UNIT                  PIC Z9.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-BASE-RENT             PIC $$$$.99.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  O-TENANTS               PIC 9.
           05  FILLER                  PIC XXXX    VALUE SPACES.
           05  O-TENANT-CHARGE         PIC $$$$.99.
           05  FILLER                  PIC XXXX    VALUE SPACES.
           05  O-DISC-PREM             PIC $$,$$$.99+.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  O-SUBTOTAL              PIC $$,$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-ELECTRIC              PIC $$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-GAS                   PIC $$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-WATER                 PIC $$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-GARBAGE               PIC $$$.99.
           05  FILLER                  PIC XXX     VALUE SPACES.
           05  O-UTILITIES             PIC $$,$$$.99.
           05  FILLER                  PIC XXXX    VALUE SPACES.
           05  O-RENT-DUE              PIC $$,$$$.99.
           05  O-OVER1000              PIC XXX.
       01  GRAND-TOTAL-LINE.
           05  FILLER                  PIC X(13)
                   VALUE 'GRAND TOTALS:'.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  O-GT-BASE-RENT          PIC $$$,$$$.99.
           05  FILLER                  PIC X(5)    VALUE SPACES.
           05  O-GT-TENANT-CHARGE      PIC $$$,$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-GT-DISCOUNT           PIC $$$$,$$$.99+.
           05  FILLER                  PIC X       VALUE SPACE.
           05  O-GT-SUBTOTAL           PIC $$$$,$$$.99.
           05  FILLER                  PIC X(36)   VALUE SPACES.
           05  O-GT-UTILITIES          PIC $$$$,$$$.99.
           05  FILLER                  PIC XX      VALUE SPACES.
           05  O-GT-RENT-DUE           PIC $$$$,$$$.99.
           05  FILLER                  PIC XXX     VALUE SPACES.
       01  DISCOUNT-LINE.
           05  FILLER                  PIC X(34)   VALUE SPACES.
           05  FILLER                  PIC X(19)
                   VALUE 'RENTALS DISCOUNTED '.
           05  O-DISC-CTR              PIC ZZ9.
       01  PREMIUM-LINE.
           05  FILLER                  PIC X(37)   VALUE SPACES.
           05  FILLER                  PIC X(16)
                   VALUE 'PREMIUM RENTALS '.
           05  O-PREM-CTR              PIC ZZ9.
       PROCEDURE DIVISION.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-CONTROL
               UNTIL EOF = 'YES'.
           PERFORM L2-CLOSING.
           STOP RUN.
       L2-INIT.
           MOVE FUNCTION CURRENT-DATE   TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH              TO H1-MONTH.
           MOVE CURRENT-DAY                TO H1-DAY.
           MOVE CURRENT-YEAR               TO H1-YEAR.
           OPEN INPUT RENT-ORDER.
           OPEN OUTPUT RENT-SALES.
           PERFORM L3-READ.
           PERFORM L4-HEADINGS.
       L2-CONTROL.
           PERFORM L3-CALCS.
           PERFORM L3-MOVE-PRINT.
           PERFORM L3-READ.
       L2-CLOSING.
           MOVE C-GT-BASE-RENT TO O-GT-BASE-RENT.
           MOVE C-GT-TENANT TO O-GT-TENANT-CHARGE.
           MOVE C-GT-DISC-PREM TO O-GT-DISCOUNT.
           MOVE C-GT-SUBTOTAL TO O-GT-SUBTOTAL.
           MOVE C-GT-UTILTIES TO O-GT-UTILITIES.
           MOVE C-GT-RENT TO O-GT-RENT-DUE.
           MOVE C-DISC-CTR TO O-DISC-CTR.
           MOVE C-PREM-CTR TO O-PREM-CTR.
           WRITE PRTLINE FROM GRAND-TOTAL-LINE
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM DISCOUNT-LINE
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM PREMIUM-LINE
               AFTER ADVANCING 1 LINE.
           CLOSE RENT-ORDER.
           CLOSE RENT-SALES.
       L3-CALCS.
           MOVE ZERO TO C-DISCOUNT.
           MOVE ZERO TO C-PREMIUM.
           MOVE SPACES TO O-OVER1000.
           EVALUATE TRUE
               WHEN UNIT1-8
                   MOVE 650                TO C-BASE-RATE
                   IF I-TENANTS > 4
                      MOVE 83.45 TO C-TENANT-CHARGE
                   ELSE IF I-TENANTS > 1
                       COMPUTE C-TENANT-CHARGE ROUNDED
                       = (I-TENANTS - 1)*25.00
                   ELSE 
                       MOVE ZERO TO C-TENANT-CHARGE
                   END-IF
               WHEN UNIT9-16
                   MOVE 700                TO C-BASE-RATE
                   IF I-TENANTS > 4
                      MOVE 135 TO C-TENANT-CHARGE
                   ELSE IF I-TENANTS > 1
                       COMPUTE C-TENANT-CHARGE ROUNDED
                       = (I-TENANTS - 1)*35.55
                   ELSE 
                       MOVE ZERO TO C-TENANT-CHARGE
                   END-IF
               WHEN UNIT17-25
                   MOVE 825                TO C-BASE-RATE
                   IF I-TENANTS > 4
                      MOVE 185.60 TO C-TENANT-CHARGE
                   ELSE IF I-TENANTS > 1
                       COMPUTE C-TENANT-CHARGE ROUNDED
                       = (I-TENANTS - 1)*50.00
                   ELSE 
                       MOVE ZERO TO C-TENANT-CHARGE
                       END-IF
                   END-IF
           END-EVALUATE.
           IF R7YTPP
               IF UNIT2325
                   COMPUTE C-PREMIUM ROUNDED = (C-BASE-RATE+
                   C-TENANT-CHARGE)*.12
                   ADD 1 TO C-PREM-CTR
               END-IF
           END-IF
           IF BPCT
               COMPUTE C-DISCOUNT ROUNDED = (C-BASE-RATE+ 
               C-TENANT-CHARGE)*.33
               ADD 1 TO C-DISC-CTR
           END-IF
           IF IAJK
               IF JULY-DECEMBER
                   COMPUTE C-BASE-RATE ROUNDED = C-BASE-RATE * .5
               END-IF
           END-IF.
           COMPUTE C-SUBTOTAL = C-BASE-RATE + C-TENANT-CHARGE +
                   C-PREMIUM - C-DISCOUNT.
           COMPUTE C-UTILITY = I-ELECTRIC+I-WATER+I-GAS+I-GARBAGE.
           COMPUTE C-TOTAL-RENT = C-SUBTOTAL + C-UTILITY.
           COMPUTE C-DISC-PREM = C-PREMIUM - C-DISCOUNT.
           IF C-TOTAL-RENT > 1000
               MOVE '***' TO O-OVER1000
           END-IF.
           PERFORM L4-GRAND-TOTAL.
       L3-MOVE-PRINT.
           PERFORM L4-BUILDING.
           MOVE I-UNIT TO O-UNIT.
           MOVE C-BASE-RATE TO O-BASE-RENT.
           MOVE I-TENANTS TO O-TENANTS.
           MOVE C-TENANT-CHARGE TO O-TENANT-CHARGE.
           MOVE C-DISC-PREM TO O-DISC-PREM.
           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE I-ELECTRIC TO O-ELECTRIC.
           MOVE I-GAS TO O-GAS.
           MOVE I-WATER TO O-WATER.
           MOVE I-GARBAGE TO O-GARBAGE.
           MOVE C-UTILITY TO O-UTILITIES.
           MOVE C-TOTAL-RENT TO O-RENT-DUE.
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES.
       L3-READ.
           READ RENT-ORDER
               AT END 
                   MOVE 'YES' TO EOF.
       L4-HEADINGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM REPORT-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM HEADING1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM HEADING2
               AFTER ADVANCING 1 LINE.
       L4-GRAND-TOTAL.
           COMPUTE C-GT-BASE-RENT = C-GT-BASE-RENT + C-BASE-RATE.
           COMPUTE C-GT-TENANT = C-GT-TENANT + C-TENANT-CHARGE.
           COMPUTE C-GT-DISC-PREM = C-GT-DISC-PREM + C-DISC-PREM.
           COMPUTE C-GT-SUBTOTAL = C-GT-SUBTOTAL + C-SUBTOTAL.
           COMPUTE C-GT-UTILTIES = C-GT-UTILTIES + C-UTILITY.
           COMPUTE C-GT-RENT = C-GT-RENT + C-TOTAL-RENT.
       L4-BUILDING.
           EVALUATE I-BLD-CODE
               WHEN 'AA'
                   MOVE 'PALACE PLACE'     TO O-RENTAL-BUILDING
               WHEN 'GG'
                   MOVE 'GEORGIA'          TO O-RENTAL-BUILDING
               WHEN 'PP'
                   MOVE 'PARK PLACE'       TO O-RENTAL-BUILDING
               WHEN 'IA'
                   MOVE 'IOWA CONDO'       TO O-RENTAL-BUILDING
               WHEN 'MS'
                   MOVE 'MARKET STREET'    TO O-RENTAL-BUILDING
               WHEN 'HH'
                   MOVE 'HIGH TOWER'       TO O-RENTAL-BUILDING
               WHEN 'R7'
                   MOVE 'UPTOWN CONDOS'    TO O-RENTAL-BUILDING
               WHEN 'GM'
                   MOVE 'GANDER MOUNTAIN'  TO O-RENTAL-BUILDING
               WHEN 'BP'
                   MOVE 'BENTON PLACE'     TO O-RENTAL-BUILDING
               WHEN 'GA'
                   MOVE 'GRAND AVENUE'     TO O-RENTAL-BUILDING
               WHEN 'JK'
                   MOVE "JACK'S PLACE"     TO O-RENTAL-BUILDING
               WHEN 'UN'
                   MOVE 'UNDERGROUND SAM'  TO O-RENTAL-BUILDING
               WHEN 'YD'
                   MOVE 'YANKEE DOODLE'    TO O-RENTAL-BUILDING
               WHEN 'YT'
                   MOVE 'YAHTZEE AVE'      TO O-RENTAL-BUILDING
               WHEN 'CP'
                   MOVE 'COURT PLACE'      TO O-RENTAL-BUILDING
               WHEN 'NZ'
                   MOVE 'NEW ZOO'          TO O-RENTAL-BUILDING
               WHEN 'VV'
                   MOVE 'VERMONT'          TO O-RENTAL-BUILDING
               WHEN 'CT'
                   MOVE 'CHINA TOWN'       TO O-RENTAL-BUILDING
               WHEN 'YS'
                   MOVE 'YORKSHIRE'        TO O-RENTAL-BUILDING
               WHEN 'ME'
                   MOVE 'MAINE APT'        TO O-RENTAL-BUILDING
           END-EVALUATE.
       END PROGRAM CBLBJC05.