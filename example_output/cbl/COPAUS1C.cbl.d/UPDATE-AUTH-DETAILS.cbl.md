```cobol
       UPDATE-AUTH-DETAILS.

           MOVE WS-FRAUD-AUTH-RECORD           TO PENDING-AUTH-DETAILS
           DISPLAY 'RPT DT: ' PA-FRAUD-RPT-DATE

           EXEC DLI REPL USING PCB(PAUT-PCB-NUM)
                SEGMENT (PAUTDTL1)
                FROM (PENDING-AUTH-DETAILS)
           END-EXEC

           MOVE DIBSTAT                        TO IMS-RETURN-CODE
           EVALUATE TRUE
               WHEN STATUS-OK
                  PERFORM TAKE-SYNCPOINT
                  IF PA-FRAUD-REMOVED
                     MOVE 'AUTH FRAUD REMOVED...'   TO WS-MESSAGE
                  ELSE
                     MOVE 'AUTH MARKED FRAUD...'    TO WS-MESSAGE
                  END-IF
               WHEN OTHER
                  PERFORM ROLL-BACK

                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while FRAUD Tagging, ROLLBACK||'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE
           .
```
