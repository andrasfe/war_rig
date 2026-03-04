```cobol
              IF ACCTIDI OF COPAU0AI IS NOT NUMERIC
                MOVE LOW-VALUES               TO WS-ACCT-ID

                MOVE 'Y'                      TO WS-ERR-FLG
                MOVE
                'Acct Id must be Numeric ...' TO WS-MESSAGE

                MOVE -1                       TO ACCTIDL OF COPAU0AI

              ELSE
                MOVE ACCTIDI OF COPAU0AI      TO WS-ACCT-ID
                                                 CDEMO-ACCT-ID
                EVALUATE TRUE
                  WHEN SEL0001I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(1)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0002I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(2)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0003I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(3)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0004I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(4)
                                             TO CDEMO-CPVS-PAU-SELECTED
```
