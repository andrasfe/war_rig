```cobol
       INITIALIZE-AUTH-DATA.
      *****************************************************************

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 5
             EVALUATE WS-IDX
               WHEN 1
                   MOVE DFHBMPRO TO SEL0001A OF COPAU0AI
                   MOVE SPACES   TO TRNID01I OF COPAU0AI
                   MOVE SPACES   TO PDATE01I OF COPAU0AI
                   MOVE SPACES   TO PTIME01I OF COPAU0AI
                   MOVE SPACES   TO PTYPE01I OF COPAU0AI
                   MOVE SPACES   TO PAPRV01I OF COPAU0AI
                   MOVE SPACES   TO PSTAT01I OF COPAU0AI
                   MOVE SPACES   TO PAMT001I OF COPAU0AI
               WHEN 2
                   MOVE DFHBMPRO TO SEL0002A OF COPAU0AI
                   MOVE SPACES   TO TRNID02I OF COPAU0AI
                   MOVE SPACES   TO PDATE02I OF COPAU0AI
                   MOVE SPACES   TO PTIME02I OF COPAU0AI
                   MOVE SPACES   TO PTYPE02I OF COPAU0AI
                   MOVE SPACES   TO PAPRV02I OF COPAU0AI
                   MOVE SPACES   TO PSTAT02I OF COPAU0AI
                   MOVE SPACES   TO PAMT002I OF COPAU0AI
               WHEN 3
                   MOVE DFHBMPRO TO SEL0003A OF COPAU0AI
                   MOVE SPACES   TO TRNID03I OF COPAU0AI
                   MOVE SPACES   TO PDATE03I OF COPAU0AI
                   MOVE SPACES   TO PTIME03I OF COPAU0AI
                   MOVE SPACES   TO PTYPE03I OF COPAU0AI
                   MOVE SPACES   TO PAPRV03I OF COPAU0AI
                   MOVE SPACES   TO PSTAT03I OF COPAU0AI
                   MOVE SPACES   TO PAMT003I OF COPAU0AI
               WHEN 4
                   MOVE DFHBMPRO TO SEL0004A OF COPAU0AI
                   MOVE SPACES   TO TRNID04I OF COPAU0AI
                   MOVE SPACES   TO PDATE04I OF COPAU0AI
                   MOVE SPACES   TO PTIME04I OF COPAU0AI
                   MOVE SPACES   TO PTYPE04I OF COPAU0AI
                   MOVE SPACES   TO PAPRV04I OF COPAU0AI
                   MOVE SPACES   TO PSTAT04I OF COPAU0AI
                   MOVE SPACES   TO PAMT004I OF COPAU0AI
               WHEN 5
                   MOVE DFHBMPRO TO SEL0005A OF COPAU0AI
                   MOVE SPACES   TO TRNID05I OF COPAU0AI
                   MOVE SPACES   TO PDATE05I OF COPAU0AI
                   MOVE SPACES   TO PTIME05I OF COPAU0AI
                   MOVE SPACES   TO PTYPE05I OF COPAU0AI
                   MOVE SPACES   TO PAPRV05I OF COPAU0AI
                   MOVE SPACES   TO PSTAT05I OF COPAU0AI
                   MOVE SPACES   TO PAMT005I OF COPAU0AI
               WHEN OTHER
                   CONTINUE
             END-EVALUATE
           END-PERFORM
           .

```
