    ,   k820309    s          18.0        -Mb                                                                                                          
       runsum.f90 AR1MA              TAR1MA                                                     
                            @                              
                         @               A                'ø                    #NDX    #NSTEP    #BUFFER    #STATE    #DATAPTR    #INIT 	   #UPDATE    #UPDATE_STATE                 $                                                                                                                              0                 $                                                                                                                             0               $                                                          	            &                   &                                                      $                                         h                 	            &                                                       $                                         °                 
            &                                           1         À    $                            	                  #TRUNSUM_INIT 
   #         @     @                            
                    #SELF    #NX    #ND    #DATAPTR              
                                     ø               #TRUNSUM              
                                                       
                                                                                                        
               &                                           1         À    $                                              #TRUNSUM_UPDATE    #         @     @                                                #SELF    #NEWVALUE              
                                     ø               #TRUNSUM              
                                                   
              &                                           1         À    $                                              #TRUNSUM_UPDATE_STATE    #         @     @                                                #SELF    #PNEW    #NX    #ND              
                                     ø               #TRUNSUM                                                               
               &                                                     
                                                       
                                                              @                               '                                          @                              'P                   #TRUNSUM    #WEIGHTS    #A1    #B1    #SETPAR    #UPDATE_STATE %                 $                                   ø                      #TRUNSUM                $                                         ø                 
            &                                                        $                                  @        
                                               
                                 0                 $                                  H        
                                               
                       ð?        1    1         À    $                                              #TAR1MA_SETPAR     #         @     @                                                  #SELF !   #PWEIGHTS "   #A1 #   #B1 $             
D                                !     P              #TAR1MA              P                              "                   
               &                                                      @                              #     
                  @                              $     
       1         À    $                           %                  #TAR1MA_UPDATE_STATE &   #         @     @                             &                    #SELF '   #PNEW (   #NX )   #ND *             
D                                '     P              #TAR1MA                                            (                   
               &                                                     
                                  )                     
  @                               *                        fn#fn    ¹      b   uapp(AR1MA    Ð   @   J  PRECISION      @   J  RUNSUM    P  °       TRUNSUM+RUNSUM #      ¥   a   TRUNSUM%NDX+RUNSUM %   ¥  ¥   a   TRUNSUM%NSTEP+RUNSUM &   J  ¬   a   TRUNSUM%BUFFER+RUNSUM %   ö     a   TRUNSUM%STATE+RUNSUM '        a   TRUNSUM%DATAPTR+RUNSUM $     Z   a   TRUNSUM%INIT+RUNSUM $   x  o      TRUNSUM_INIT+RUNSUM )   ç  U   a   TRUNSUM_INIT%SELF+RUNSUM '   <  @   a   TRUNSUM_INIT%NX+RUNSUM '   |  @   a   TRUNSUM_INIT%ND+RUNSUM ,   ¼     a   TRUNSUM_INIT%DATAPTR+RUNSUM &   H  \   a   TRUNSUM%UPDATE+RUNSUM &   ¤  `      TRUNSUM_UPDATE+RUNSUM +     U   a   TRUNSUM_UPDATE%SELF+RUNSUM /   Y     a   TRUNSUM_UPDATE%NEWVALUE+RUNSUM ,   å  b   a   TRUNSUM%UPDATE_STATE+RUNSUM ,   G	  l      TRUNSUM_UPDATE_STATE+RUNSUM 1   ³	  U   a   TRUNSUM_UPDATE_STATE%SELF+RUNSUM 1   
     a   TRUNSUM_UPDATE_STATE%PNEW+RUNSUM /   
  @   a   TRUNSUM_UPDATE_STATE%NX+RUNSUM /   Ô
  @   a   TRUNSUM_UPDATE_STATE%ND+RUNSUM '     P       #UNLPOLY+ISO_C_BINDING    d         TAR1MA    ü  ]   a   TAR1MA%TRUNSUM    Y     a   TAR1MA%WEIGHTS    í  ¥   a   TAR1MA%A1      ¥   a   TAR1MA%B1    7  [   a   TAR1MA%SETPAR      p      TAR1MA_SETPAR #     T   a   TAR1MA_SETPAR%SELF '   V     a   TAR1MA_SETPAR%PWEIGHTS !   â  @   a   TAR1MA_SETPAR%A1 !   "  @   a   TAR1MA_SETPAR%B1 $   b  a   a   TAR1MA%UPDATE_STATE $   Ã  l      TAR1MA_UPDATE_STATE )   /  T   a   TAR1MA_UPDATE_STATE%SELF )        a   TAR1MA_UPDATE_STATE%PNEW '     @   a   TAR1MA_UPDATE_STATE%NX '   O  @   a   TAR1MA_UPDATE_STATE%ND 