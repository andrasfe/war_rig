```cobol
                                          TO CDEMO-CPVS-AUTH-KEYS(1)

                   MOVE PA-TRANSACTION-ID TO TRNID01I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE01I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME01I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE01I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV01I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT01I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT001I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0001A OF COPAU0AI
               WHEN 2
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(2)

                   MOVE PA-TRANSACTION-ID TO TRNID02I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE02I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME02I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE02I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV02I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT02I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT002I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0002A OF COPAU0AI
               WHEN 3
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(3)

                   MOVE PA-TRANSACTION-ID TO TRNID03I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE03I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME03I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE03I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV03I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT03I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT003I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0003A OF COPAU0AI
               WHEN 4
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(4)

                   MOVE PA-TRANSACTION-ID TO TRNID04I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE04I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME04I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE04I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV04I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT04I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT004I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0004A OF COPAU0AI
               WHEN 5
                   MOVE PA-AUTHORIZATION-KEY
                                          TO CDEMO-CPVS-AUTH-KEYS(5)

                   MOVE PA-TRANSACTION-ID TO TRNID05I OF COPAU0AI
                   MOVE WS-AUTH-DATE      TO PDATE05I OF COPAU0AI
                   MOVE WS-AUTH-TIME      TO PTIME05I OF COPAU0AI
                   MOVE PA-AUTH-TYPE      TO PTYPE05I OF COPAU0AI
                   MOVE WS-AUTH-APRV-STAT TO PAPRV05I OF COPAU0AI
                   MOVE PA-MATCH-STATUS   TO PSTAT05I OF COPAU0AI
                   MOVE WS-AUTH-AMT       TO PAMT005I OF COPAU0AI
                   MOVE DFHBMUNP          TO SEL0005A OF COPAU0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
```
