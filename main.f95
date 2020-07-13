
PROGRAM WAVE_PML
	!#################################
	USE MOD_PARAM;
    USE MOD_MODEL;
    USE MOD_READ_WRITE;			   
    USE MOD_PML;				
    USE MOD_GEOMETRY;			
    USE MOD_SOURCE;
    USE MOD_CALCULATE;
	
    USE MOD_UTILITIES;
    USE MOD_ALLOCATE_MAT_VEC;
 
	!#########################################################    
    IMPLICIT NONE
    TYPE(MODEL) 		:: M,MPAD   ! GIVEN MODEL, PADDED MODEL
    TYPE(BOUNDARY_PML) 	:: BC
    TYPE(SOURCE)		:: S
    TYPE(GEOMETRY_SNR)  :: G
    INTEGER	:: I,J
    REAL(KIND=DP)		:: SFX,SLX,SDX,SZ,RFX,RLX,RDX,RZ
    REAL(KIND=DP), ALLOCATABLE	:: MATOUT(:,:)

    REAL(KIND=DP),ALLOCATABLE	:: DPT(:),VP_VEC(:),RHO_VEC(:)

   	!#########################################################    
    !DPT=(/40.,90.,150.,200./);	  RHO_VEC=(/1800.,1850.,1900.,1950./);	VP_VEC=(/1500.,1600.,1800.,2000./)	
    DPT=(/1000.,2000./);		 	
    RHO_VEC=(/1800.,2000./);		
    VP_VEC=(/1800.,2000./);		
    
	!#########################################################
    I=2
    CALL READ_IP_VAR_REAL(I,M%XMAX);       I=I+1  
    CALL READ_IP_VAR_REAL(I,M%ZMAX);       I=I+1
    CALL READ_IP_VAR_INT(I,M%NX);         I=I+1
    CALL READ_IP_VAR_INT(I,M%NZ);         I=I+1
    CALL READ_IP_VAR_REAL(I,M%DH);        

    !############################ CREATE/READ MODEL #############################

    !!!!!!!!!!---------------WITH LAYERED MODEL (MODEL CHOICE 1)------------------!!!!!!!!!!!!!
    CALL MODEL_N_LAYER(MAT=M%VP,    NX=M%NX,           NZ=M%NZ, &
    				   DH=M%DH,     HTV_RATIO=1.0_DP,  DEPTHV=DPT,  VEC=VP_VEC,   FNAME='VP',  PLOTON='N');    
   
    CALL MODEL_N_LAYER(MAT=M%RHO,   NX=M%NX,           NZ=M%NZ, &
                       DH=M%DH,     HTV_RATIO=1.0_DP,  DEPTHV=DPT,  VEC=RHO_VEC,  FNAME='RHO',  PLOTON='N');    
                       
	!CALL PLT_MAT(M%VP,'MODEL VP','X','Z',3);		!CALL PRINT_MAT('VP MATRIX : ', M%VP)                       
    !CALL PLT_MAT(M%RHO,'MODEL RHO','X','Z',3);			!CALL PFILE('VP.TXT','MODEL','X','Z',10)    

    !!!!!!!!!!--------------- WITH PROVIDED MODEL (MODEL CHOICE 2) ------------------!!!!!!!!!!!!!      
	!CALL MODEL_READ_FILE(M%VP,'VP.TXT',M%NX,M%NZ,'N');    
	!CALL PLT_MAT(M%VP,'MODEL','X','Z',5)               !CALL PLT_MAT_F('VP.TXT','MODEL','X','Z',5)
    
    !CALL MODEL_READ_FILE(M%RHO,'RHO.TXT',M%NX,M%NZ,'N');
    !CALL PLT_MAT(M%RHO,'MODEL','X','Z',5)              !CALL PLT_MAT_F('RHO.TXT','MODEL','X','Z',5)   

    !##########################################################
    
    I=9             !PREPARE BOUNDARIES
    CALL READ_IP_VAR_INT(I,BC%NAB);     I=I+1  ! READ NO OF LAYERS
    BC%TOPBC= READ_IP_VAR_STR(10)              ! READ TYPE OF TOP BOUNDARY 
    CALL PML_2D(M,BC,PLOTON='N');              ! CREATE TYPE BOUNDARY ACCORDING TO SEPECS    
	
		
    !#########################################################
    
    !!!! ---------PAD/ADD EXTRA LAYER TO MODEL ACCORDING TO ABSORBING BOUNDARIES----------------!!!
    CALL PAD_ARRAY(MPAD%VP,  M%VP,  BC%NAB, BC%TOPBC, PLOTON='N',  NX=MPAD%NX, NZ=MPAD%NZ);    
    CALL PAD_ARRAY(MPAD%RHO, M%RHO, BC%NAB, BC%TOPBC, PLOTON='N');  
    MPAD%DH=M%DH;           
        
    !!!!----------CREATE INTERPOLATED MODEL SUBROUTINE IN UTILITIES-------!!!!
    CALL INTERPOLATE_MODEL(MPAD%VP,MPAD%VPXH,MPAD%VPZH,'N');    !CALL PRINT_MAT('INTERPOLATED MAT VPZH : ',MPAD%VPZH )
    CALL INTERPOLATE_MODEL(MPAD%RHO,MPAD%RHOXH,MPAD%RHOZH,'N'); !CALL PRINT_MAT('INTERPOLATED MAT RHOZH : ',MPAD%RHOZH )    


	!############################################################
        
    I=13           !CREATE SOURCE WAVELET
    CALL READ_IP_VAR_REAL(I,S%DT);        I=I+1
    CALL READ_IP_VAR_REAL(I,S%F0);        I=I+1
    CALL READ_IP_VAR_REAL(I,S%T0);        I=I+1
    CALL READ_IP_VAR_REAL(I,S%T);         I=I+1
    S%NAME = READ_IP_VAR_STR(17)                    !PRINT*, 'CHECK :', S%NAME

    CALL SOURCE_RICKER(S);       
    !CALL   PLT_VEC(S%SIG(1:100),'SOURCE','N','AMPLITUDE',3)    !CALL PRINT_VEC('SOURCE VECTOR: ', S%SIG)
	
	!############################################################
        
    I=20        !Source Geometry
    CALL READ_IP_VAR_REAL(I,SFX);       I=I+1
    CALL READ_IP_VAR_REAL(I,SLX);       I=I+1
    CALL READ_IP_VAR_REAL(I,SDX);       I=I+1
    CALL READ_IP_VAR_REAL(I,SZ);        I=I+1 
    CALL GEOMETRY_SRC_SURF_STLINE(BC, SFX, SLX, SDX, SZ, G,MPAD)
    
    I=26        !Reciever Geometry
    CALL READ_IP_VAR_REAL(I,RFX);       I=I+1
    CALL READ_IP_VAR_REAL(I,RLX);       I=I+1
    CALL READ_IP_VAR_REAL(I,RDX);       I=I+1
    CALL READ_IP_VAR_REAL(I,RZ);        I=I+1
    CALL GEOMETRY_REC_SURF_STLINE(BC, RFX, RLX, RDX, RZ, G,M)
	
	!############################################################
        
	!CALL CHECKING(S,MPAD)
    IF( MAXVAL(M%VP)/(M%DH/S%DT)<1 )THEN
       PRINT*,'DT AND DX ARE OK'
       ELSE
         PRINT*,'CHANGE DT OR DX'
    END IF
	
	!############################################################
        
    !REMAINING : CALCULATION SUBROUTINE
    DO I=1,G%NSRC
        !CALL CALCULATE_ACOUSTIC1(MPAD,BC,S,G,I,PLOTON='y',NFIG=40)    
        !CALL CALCULATE_ACOUSTIC2(MPAD,BC,S,G,I,PLOTON='Y',NFIG=400)        
        !CALL CALCULATE_ACOUSTIC_PML(MPAD,BC,S,G,I,PLOTON='Y',NFIG=400)        
		CALL CALCULATE_ACOUSTIC_PML_FD_LELE(MPAD,BC,S,G,I,PLOTON='Y',NFIG=1)        
    END DO
    
END PROGRAM WAVE_PML