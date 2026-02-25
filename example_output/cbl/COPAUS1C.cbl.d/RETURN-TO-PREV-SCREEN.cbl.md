```cobol
           EVALUATE TRUE
               WHEN STATUS-OK
                  SET AUTHS-NOT-EOF              TO TRUE
               WHEN SEGMENT-NOT-FOUND
               WHEN END-OF-DB
                  SET AUTHS-EOF                  TO TRUE
               WHEN OTHER
                  MOVE 'Y'     TO WS-ERR-FLG

                  STRING
                  ' System error while reading next Auth: Code:'
                  IMS-RETURN-CODE
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  PERFORM SEND-AUTHVIEW-SCREEN
           END-EVALUATE
           .

       UPDATE-AUTH-DETAILS.
```
