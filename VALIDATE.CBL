       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VALIDATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTIONS
               ASSIGN TO 'SOURCE6.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTIONS-REPORT
               ASSIGN TO 'REPORT.DOC'
                   ORGANIZATION IS LINE SEQUENTIAL.

       FD  TRANSACTIONS.
       01                              PIC X(80).
       FD  TRANSACTIONS-REPORT.
       01  PRINT-LINE                  PIC X(133).

       WORKING-STORAGE SECTION.
       01  WORKING-VARIABLES.
           05  EOF-WS                  PIC XXX      VALUE 'NO'.
           05  RECORD-NUMBER-WS        PIC 999      VALUE ZERO.
           05  TOTAL-RECORD-COUNT-WS   PIC S9(5)    VALUE ZERO.

       01  BAD-DATA-RECORD-FIELDS.
           05  RECORD-TYPE-IN          PIC X.
               88 REC-TYPE-VALID       VALUE 'A' 'E' 'T'.
           05  ACCOUNT-CODE-IN         PIC X(4).
           05                          PIC X(4).
           05  AMOUNT-IN               PIC S9(6)V99.
           05  AMOUNT-IN-X             PIC X(8)
               REDEFINES               AMOUNT-IN.
           05  TRANSACTION-DATE-IN.
               10  MONTH-IN            PIC 99.
               10  MONTH-IN-X          PIC XX
                   REDEFINES           MONTH-IN.

               10  DAY-IN              PIC 99.
               10  DAY-IN-X            PIC XX
                   REDEFINES           DAY-IN.

               10  YEAR-IN             PIC 9999.
                   88   YEAR-IN-VALID  VALUE 2001 THRU 2009.
               10  YEAR-IN-X           PIC X(4)
                   REDEFINES           YEAR-IN.

       01  COLUMN-HEADER1.
           05                          PIC X.
           05                          PIC X(18) VALUE 'RECORD'.
           05                          PIC X(36) VALUE 'DUMP OF'.
           05                          PIC X(5)  VALUE 'ERROR'.

       01  COLUMN-HEADER2.
           05                          PIC X.
           05                          PIC X(16) VALUE 'NUMBER'.
           05                          PIC X(37) VALUE 'INPUT DATA'.
           05                          PIC X(7)  VALUE 'MESSAGE'.

       01  DETAIL-LINE.
           05                          PIC X.
           05  RECORD-NUM-OUT          PIC ZZZ9.
           05                          PIC X(5).
           05  RECORD-TYPE-OUT         PIC X.
           05                          PIC X.
           05  ACCOUNT-CODE-OUT        PIC XXXX.
           05                          PIC X.
           05  AMOUNT-OUT              PIC X(8).
           05                          PIC X.
           05  TRANSACTION-DATE-OUT    PIC X(8).
           05                          PIC X(5).
           05  ERROR-MESSAGE-OUT       PIC X(89).

       01 FOOTER-LINE.
           05                          PIC X.
           05                          PIC X(30) VALUE
                                           'TOTAL BAD RECORDS FOUND: '.
           05  TOTAL-RECORD-COUNT-OUT  PIC Z9(5).

       PROCEDURE DIVISION.
       100-MAINLINE.
           PERFORM 200-OPEN.
           PERFORM 300-PROCESS UNTIL EOF-WS = 'YES'.
           PERFORM 800-FOOTER.
           PERFORM 900-CLOSE.
           STOP RUN.

       200-OPEN.
           OPEN INPUT TRANSACTIONS
                OUTPUT TRANSACTIONS-REPORT.
            PERFORM 250-READ.
            PERFORM 700-HEADER.

       250-READ.
           READ TRANSACTIONS INTO BAD-DATA-RECORD-FIELDS
               AT END MOVE 'YES' TO EOF-WS
           END-READ.


       300-PROCESS.
           PERFORM 350-INITIALIZE.
           PERFORM 400-CHECK-FOR-VALID-REC-TYPE.
           PERFORM 405-CHECK-FOR-BLANK-ACCT.
           PERFORM 410-CHECK-FOR-NUM-DAY.
           PERFORM 415-CHECK-FOR-NUM-MONTH.
           PERFORM 420-CHECK-FOR-NUM-YEAR.
           PERFORM 425-CHECK-FOR-NUM-AMOUNT.
           PERFORM 430-CHECK-FOR-VALID-YEAR.
           PERFORM 435-CHECK-FOR-NEG-AMOUNT.
           PERFORM 250-READ.

       350-INITIALIZE.
           ADD 1 TO RECORD-NUMBER-WS.

       400-CHECK-FOR-VALID-REC-TYPE.
           IF  REC-TYPE-VALID
               THEN CONTINUE
               ELSE MOVE 'RECORD TYPE IS INVALID' TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       405-CHECK-FOR-BLANK-ACCT.
           IF ACCOUNT-CODE-IN <> SPACES
               THEN CONTINUE
               ELSE MOVE 'ACCOUNT CODE EXP CANT BE BLANK' TO
                       ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       410-CHECK-FOR-NUM-DAY.
           IF DAY-IN IS NOT NUMERIC
               THEN MOVE 'DAY IN IS NOT NUMERIC' TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.


       415-CHECK-FOR-NUM-MONTH.
           IF MONTH-IN IS NOT NUMERIC
               THEN MOVE 'MONTH IN IS NOT NUMERIC' TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       420-CHECK-FOR-NUM-YEAR.
           IF YEAR-IN IS NOT NUMERIC
               THEN MOVE 'YEAR IN IS NOT NUMERIC' TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       425-CHECK-FOR-NUM-AMOUNT.
           IF AMOUNT-IN IS NOT NUMERIC
               THEN MOVE 'AMOUNT EXP IN IS NOT NUMERIC'
                       TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       430-CHECK-FOR-VALID-YEAR.
           IF YEAR-IN IS NUMERIC AND YEAR-IN-VALID
               THEN CONTINUE
               ELSE MOVE 'YEAR NOT 2001-2009' TO ERROR-MESSAGE-OUT
                    PERFORM 350-PRINT
           END-IF.

       435-CHECK-FOR-NEG-AMOUNT.
           IF AMOUNT-IN IS NUMERIC
               IF AMOUNT-IN IS NOT NEGATIVE
                   THEN CONTINUE
                   ELSE MOVE 'AMOUNT EXP IS NEGATIVE'
                           TO ERROR-MESSAGE-OUT
                        PERFORM 350-PRINT
               END-IF
           END-IF.


       350-PRINT.
           MOVE RECORD-NUMBER-WS          TO RECORD-NUM-OUT.
           MOVE RECORD-TYPE-IN            TO RECORD-TYPE-OUT.

           MOVE ACCOUNT-CODE-IN           TO ACCOUNT-CODE-OUT.
           MOVE AMOUNT-IN-X               TO AMOUNT-OUT.
           MOVE TRANSACTION-DATE-IN       TO TRANSACTION-DATE-OUT.

           WRITE PRINT-LINE FROM DETAIL-LINE AFTER 1.

           ADD 1 TO TOTAL-RECORD-COUNT-WS.

       700-HEADER.
           WRITE PRINT-LINE FROM COLUMN-HEADER1 AFTER PAGE.
           WRITE PRINT-LINE FROM COLUMN-HEADER2 AFTER 1.
           WRITE PRINT-LINE FROM SPACES AFTER 2.

       800-FOOTER.
           MOVE TOTAL-RECORD-COUNT-WS TO TOTAL-RECORD-COUNT-OUT
           WRITE PRINT-LINE FROM FOOTER-LINE AFTER 2.

       900-CLOSE.
           CLOSE TRANSACTIONS TRANSACTIONS-REPORT.
