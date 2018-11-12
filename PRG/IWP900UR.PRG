*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : Recuperar copia de seguridad.
*-H
*-H	Release			: 01
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref

PRIVATE ALL LIKE w_*

if parameters()=0
    w_pref=''
endif

w_alta=.f.
w_recu=.f.   && me indica si recupero el backup para indexar

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

** Variables internas
w_opcion='Confirma'
w_prog=program()
w_pfile='900ur'

** Datos del archivo a mantener
w_title='RECUPERAR COPIA DE SEGURIDAD DE '

if w_pref='C'

	set escape off
	
	wait wind 'El recupero de sistema completo debera hacerlo desde el util'

	pop key  
	pop menu _msysmenu

	read when .f.
		
else	

	set skip of menu _msysmenu .t.

	do case
		case w_pref='E'
			w_title=w_title+'EMPRESA)'
		
			if used('S02')
			   sele s02
			   use
			endif

			if used('S01')
			   sele s01
			   use
			endif

			if !used('f10')
				use &d0f10 in 0 alias f10 order p0coem
			
				if neterr()
			    	pop key  
	    			pop menu _msysmenu
    
		    		=closall()
	    			return
				endif
			endif

		case w_pref='S'
			w_title=w_title+'ARCH.SISTEMA)'
			
			=closall()
	endcase

	w_orig=1
	w_opci=2
	w_fech=ctod('')
	w_dest=sys(5)

	w_cont=0

	do iwm900uf.mpr

	do iws900ur.spr	

	pop key  
	do ssm995.mpr	&& R.01 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
	pop menu _msysmenu

endif

if !wexist('Teranor')
	=closall()
endif

return


*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    Procedure Conf900ur
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w0mop$'D'
	=advsuave('Versi๓n Demostraci๓n. Inhibido el acceso.')
	read when .f.
	return
endif

if .not. file('\iw\aunzip32.dll')
   wait ' No se encuentra el AUNZIP32.DLL en el directorio actual ' wind
   return
endif   

** Verificacion de archivo de backup y fecha del backup.
**

w_driv=left(w_path,2)	

if !dispOk(w_driv)
	=advsuave('Dispositivo Origen No Vแlido ')
 	return
endif

w_back=adir(w_file,w_path+'*.*')	

if empty(w_back)
    wait 'Unidad origen incorrecta' wind nowai
    return
endif

do case
	case w_pref='E'	&& empresa
		w_undi=ascan(w_file,d0pref+d0coem+'.ARJ')
		w_mudi=ascan(w_file,d0pref+d0coem+'VFP.001')	

	case w_pref='S'	&& arch.sistema

		w_undi=ascan(w_file,d0pref+'SIS.ARJ')
		w_mudi=ascan(w_file,d0pref+'SISVFP.001')	
endcase

if empty(w_undi) .and. empty(w_mudi)
	wait 'La unidad origen no contiene un resguardo de Sueldos y Jornales' wind time 1	
	return
endif

if empty(w_undi)
   w_fech=dtoc(w_file(w_mudi+2))+' '+w_file(w_mudi+3)
else
   w_fech=dtoc(w_file(w_undi+2))+' '+w_file(w_undi+3)
endif

do iws900ud.spr   && Muestra fecha del backup

if w_opci=2
     return   && Cancela operacion
else
     * Descompacta 
     *          ARJ X : descompacta tambien subdirectorios
     *          E	  : descompacta los archivos donde esta parado
     *          -V    : puede tener varios volumens
     *          -Y    : no pregunta si sobreescribe los archivos 
     *          -HF2  : no tiene en cuenta los nombres largos para DOS o Windows NT
     *	 	-HTdirec. : descomprime en el directorio direc.

     if sino('Confirma recupero de copia de seguridad ?','No',iif(w_pref='E','Recupero de Empresa','Recupero de Archivos de Sistema'))='S'
     
         wait 'Procesando ... <No interrumpir>' wind nowai


		w_cd=sys(5)+curdir()
		w_sys=d0pref

		do case
			case w_pref='E'	&& empresa
				w_empr=allt(d0coem)
			case w_pref='S'	&& sistema
				w_empr='SIS'
				=mkdir(w_cd+w_empr)
		endcase

		wait wind chr(13)+chr(13)+'Recuperando de : '+w_path+w_sys+w_empr+'VFP'+'.001'+chr(13)+chr(13) nowait	
						
		=mkdir(w_cd+w_empr+'\_rkmemo\')

		set defa to &w_cd

		private w_fzip,w_dest,w_vfox,w_inst001,w_inst002
	
		w_fzip=w_path+w_sys+w_empr+'VFP.001'	
		w_dest=w_cd+w_empr+'\_rkmemo'

		w_inst001='=inkey(1)'
		w_inst002='=pkunzip("'+w_fzip+'","'+w_dest+'","'+'_bkfin.dbf'+'")'
		private d0f9fe
		d0f9fe=getenv('temp')+'\'+sys(3)
		w_vfox=d0f9fe+'.mem'
		save to &w_vfox. all like w_inst0*
		release all like w_inst0*
		=visuafox('ejeproc(.f.)',w_vfox,iif(w_pref='E','Recupero de Empresa','Recupero de Archivos de Sistema'),1)

		i=0
		do while .t.
			wait wind chr(13)+chr(13)+'Recuperando de : '+w_path+w_sys+w_empr+'VFP.001'+chr(13)+replicate('',i)+chr(13) time 1
			if len('Recuperando de : '+w_path+w_sys+w_empr+'VFP.001')=i
				i=1
			else
				i=i+1
			endif
			if w_pref='E'		&& empresas
				if mfile(w_cd+w_empr+'\_rkmemo\'+'_bkfin.dbf') and mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+'Empr_Ver.dbf')
					sele 0
					use &w_cd.&w_empr.\_rkmemo\&w_sys.\Empr_Ver alias Empr_Ver
					w_p0coem=empr_ver.p0coem
					use in empr_ver
					if  dispok(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_p0coem)	&& por si existe la empresa pero no el directorio
						exit
					endif
				endif
			else
		 		if mfile(w_cd+w_empr+'\_rkmemo\'+'_bkfin.dbf') and mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+'ivf010.dbf')	
					if  dispok(w_cd+w_empr+'\_rkmemo\'+w_sys)	&& por si existe la empresa pero no el directorio
						exit
					endif
				endif
			endif
			if lastkey()=p_esc
				if sino('El recupero de '+w_path+w_sys+w_empr+'VFP.001'+' esta llevando tiempo. La demora depende del volumen de informaci๓n a descompactar. Sigue aguardando?','Si',iif(w_pref='E','Recupero de Empresa','Recupero de Archivos de Sistema'))!='S'
					=advgrave('Va a liberar el proceso, lamentablemente no va a terminar de recuperar los archivos.',iif(w_pref='E','Recupero de Empresa','Recupero de Archivos de Sistema'))
					exit
				else
					keyboard '{enter}'
					=inkey()
				endif
			endif
		 enddo

		do case
		 case w_pref='E'	&& empresa
			if mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+'Empr_Ver.dbf')
				sele 0
				use &w_cd.&w_empr.\_rkmemo\&w_sys.\Empr_Ver alias Empr_Ver
				w_p0coem=empr_ver.p0coem
				locate for p0coem=d0coem
				if !eof()
					wvein=p0veda	
					wvees=p0veda	
					wveda=p0veda
				else 
					wvein='000'
					wvees='000'
					wveda='000'		
				endif
				use in empr_ver		
			 else
				 w_p0coem=w_empr	
				 wvein='000'
				 wvees='000'
				 wveda='000'
		     endif


			if !wexist('teranor')
				=seek(d0coem,'f10')

				if .not. rlock('f10')
					=errornet(054)
					sele f10
					use
					exit
				endif
			endif

			* si esta en blanco no viene de eleccion de empresas.
			* ---------------------------------------------------
			if iif(!wexist('teranor'),lockemp(nf_coem),.t.)

				wcomver=compvers(w_cd+w_empr+'\_rkmemo\'+w_sys+'\')	

				sele f10

				if wcomver
					 if !cofi900ur('')	&& controla el resguardo para que no escrache comprob.fiscales	
					 	wait 'No recupero la copia de seguridad!!!' wind time 2 	
						=audi('RCE S','Hubo Problemas','',d0coem)
						=unlockemp(nf_coem)
						return
					 endif	 

	  				 =audi('RCE S','Exitoso','',d0coem)

				endif

				=unlockemp(nf_coem)
				
			else
				=errornet(050)
			endif

		 case w_pref='S'	&& SISTEMA

		 
			wcover=compver(w_cd+w_empr+'\_rkmemo\'+w_sys+'\')

			if wcover
				d0mene=.f.

				if !lockemp('')
					=errornet(075)
					d0mene=.t.

					wait clear
					pop key
					pop menu _msysmenu
					read when .f.

					return
				endif

				sele f77
				use

				use &d0f77 in 0 alias f77 excl
				if neterr()
					=errornet(076)
					=login('S')
					=login('E',.t.)
					=unlockemp('')
					d0mene=.t.

					wait clear
					pop key
					pop menu _msysmenu
					read when .f.

					return
				endif

				d0mene=.t.
		
				if used('S02')
					sele s02
					use
				endif

				 if !cofi900ur('')	&& controla el resguardo para que no escrache comprob.fiscales	
				 	wait 'No recupero la copia de seguridad!!!' wind time 2		
					return
				 endif	 

				=unlockemp('')

				create dbf indexar.mem (nada c(1))	&& para que genere indices de arch.sistema
				
			endif

			sele f77
			use

		 endcase

         wait 'Procesando ... <No interrumpir>' wind nowai

		 if w_pref='E'	&& empresa
		
			* ACTUALIZACION \iw\ivf010
			* ------------------------
			sele f10
			if seek(d0coem,'f10')
				if p0vees+p0veda <> wvees+wveda
					repl p0vein with wvein;
						,p0vees with wvees;
						,p0veda with wveda
				endif
			endif
			 
		endif
		         
		wait wind 'Recupero la copia de seguridad!!!' time 2
		
		wait wind 'Debe volver a ingresar al sistema.' time 2
     
		show gets disable

		* Aqui fuerzo la salida Aunque por version no haga falta
		* para que ingrese e inicialice obligadamente la empresa
		* A partir version 300 00
		*
		wait clear
         
		if set('develop')='ON'
		    pop key  
		    pop menu _msysmenu
			 clear window
			 =closall()
          cancel
		else
		    pop key  
		    pop menu _msysmenu
			 clear window
			 =closall()
          	=mquit()
		endif
		
   endif
endif

return

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    Function Cofi900ur
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_para

private w_dant,w_tmp, w_wind, w_todo

if w_pref='E'		&& empresa	

	w_dant=curdir()+d0coem+'\'

else			&& arch.sistema

	w_dant=curdir()	

endif

set resource off

**verifica acceso a cada base del directorio
if w_pref='E' 		&& empresa	
	w_tmp=w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_empr+'\'
else
	w_tmp=w_cd+w_empr+'\_rkmemo\'+w_sys+'\'
endif
if !accefile(w_tmp,'*.DBF')
	return .f.
endif

w_disi=diskspace()             && Espacio libre
w_test=adir(w_arch,w_tmp+'\*.*')   && Archivos a copiar

if w_pref='E' 	&& empresa	
	if w_test<14
		wait wind 'Error al descompactar el resguardo. Intentar nuevamente.' 
		return .f.
	endif
else	&& arch. sistema
	if w_test<10
		wait wind 'Error al descompactar el resguardo. Intentar nuevamente.' 
		return .f.
	endif
endif

w_size=0

for i=1 to w_test
   w_size=w_size+w_arch[i,2]
endfor

w_esre=w_size*2.5              && Total de espacio requerido

if w_esre > w_disi
    wait 'No hay suficiente espacio para continuar' wind
    return .f.
endif

if w_pref='E'		&& empresa	
	if wvein='000'
		w_todo=.f.	&& sino esta empresas es version vieja ni controlo comprobantes y pido clave
	else
		sele f10
		if seek(d0coem,'f10')
			do case
				case p0vees+p0veda < wvees+wveda	&& version mayor a la actual
	
					wait window 'Resguardo de una version posterior a la actual'+chr(13)+;
							'Debera actualizar primero la version del sistema' 
					return .f.
	
				case p0vees+p0veda > wvees+wveda	&& version menor a la actual pide clave

					w_todo=.f.

				case w_pref='E'

					if !file(w_tmp+'\ivf005.dbf')
						return .f.
					endif	

					sele 0
					use &w_tmp.\ivf005 exclu

					if neterr()
	    				return .f.
					endif

					index on ptoi(d5peri)+d5tipo+d5sucu+d5tifo+d5tico+d5nume tag d5peri
	
					use &w_tmp.\ivf005 order d5peri alias aux

					use &w_dant.\ivf005 in 0 alias f05 order d5peri

					if neterr()
						sele aux
						use
						w_todo=.f.	&& si f05 esta defectuoso no puedo controlar pero pido clave

						wait wind 'Intentar nuevamente.' 
						
					else

						** controlo si faltan comprobantes tanto para contr.fiscal como para impresora
						w_todo=.t.
						go top in f05
						do while !eof('f05')
							if !seek(ptoi(f05.d5peri)+f05.d5tipo+f05.d5sucu+f05.d5tifo+f05.d5tico+f05.d5nume,'aux')
								w_todo=.f.
								exit
							endif
							skip in f05
						enddo

						sele f05
						use
						sele aux
						use
					endif
				otherwise
					w_todo=.t.
	
			endcase
		endif
	endif

else	&& arch.sistema
	w_todo=.t.
	w_cont=0
	if !file(w_tmp+'\ivf010.dbf')
		return .f.
	endif	

	sele 0
	use &w_tmp.\ivf010 exclu
	index on p0coem tag p0coem

	use &w_tmp.\ivf010 order p0coem alias f10

	w_cant0=adir(dirgv0,'0??','D')
	w_cant1=adir(dirgv1,'1??','D')
	w_cant2=adir(dirgv2,'2??','D')
	w_cant3=adir(dirgv3,'3??','D')
	w_cant4=adir(dirgv4,'4??','D')
	w_cant5=adir(dirgv5,'5??','D')
	w_cant6=adir(dirgv6,'6??','D')
	w_cant7=adir(dirgv7,'7??','D')
	w_cant8=adir(dirgv8,'8??','D')
	w_cant9=adir(dirgv9,'9??','D')

	go top in f10
	do while !eof('f10')
		w_auxd='dirgv'+left(f10.p0coem,1)
		if ascan(&w_auxd, f10.p0coem)=0
			use in f10
			wait window 'El resguardo difiere con la configuracion de empresas actual'
			return .f.
		endif
		skip in f10
	enddo
	
	for j=0 to 9
		w_auxc=val('w_cant'+str(j,1))
		for i=1 to w_auxc
			w_auxd='dirgv'+str(j,1)+'[i,1]'
			if len(alltrim(w_auxd))=3		&& ej= 000
				if !seek(right(w_auxd,3), 'f10')
					use in f10
					wait window 'El resguardo difiere con la configuracion de empresas actual'
					return .f.
				endif
			endif
		endfor
	endfor
	use in f10
		
endif

if !w_todo

	** Salvado de estados anteriores 

	push key clear      && Salva estados de teclas 
	push menu _msysmenu && Salva estado de menu

	set skip of menu _msysmenu .t.

	w_cont=0		&& 0->no continua / 1->continua
	wcon=0
	wncla=spac(5)   && Nueva Clave			
	wacla=spac(5)   && Clave anterior		
	walet=spac(5)   && Aleatorio con letra	
	wopci='RECUP'   && Opciones
	wHabi='C'		&& habilitacion

	wz_sis='IW'

	if file('\iw\iw.mem')
		rest from \iw\iw.mem additive
	else
		s_text='Instalacion Incompleta, falta archivo de claves'
		do iws900am.spr
		pop key  
		pop menu _msysmenu
		return .f.
	endif

	if type('Z_HA')=='C'	&& terminal de habilitacion
		nf_ha = decript(z_ha,3)
		d0teha = subst(nf_ha,18+asc(subst(nf_ha,17,1))+1,2)
	else
		d0teha = '01'
	endif

	if p_netuser<>'01'	
		wait 'Este proceso Debe Efectuarse Desde la Terminal 1' wind
		pop key  
		pop menu _msysmenu
		return .f.
	endif

	if getenv('TESTVER')='OFF' and p_netuser='01'	&& para el caso monousuario en red
		t_cant=adir(wdius,'C:\MEMOSYS\'+d0pref+'\??','D')
		if t_cant=3
			if d0teha<>wdius[3,1]
				wait 'Este proceso Debe Efectuarse Desde la Terminal 1' wind
				pop key  
				pop menu _msysmenu
				return .f.
			endif
		endif
	endif	

	w_st=.f.
	
	if set('develop')='OFF'		
		clav1=decript(Z_NI,3)
		clav2=decript(Z_NC,3)
		clav3=decript(Z_NT,3)

		winst=subst(clav1,18+asc(subst(clav1,17,1))+1,5)	
		wacla=subst(clav2,18+asc(subst(clav2,17,1))+1,5)	
		wterm=subst(clav3,18+asc(subst(clav3,17,1))+1,4)

	else		
		winst='     '
		wacla='     '
		wterm='  99'
	endif		

	** las opciones van en blanco **

	walet=genale()
	
	wait clear

	do iws900us.spr			&& ingreso de clave
	
	pop key  
	pop menu _msysmenu
        
endif

wait clear

wait 'Procesando ... <No interrumpir>' wind nowai

if w_todo or w_cont=1
	**Puede recuperar**
	if w_pref='E' 		&& empresa	
		=copyfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_p0coem+'\',w_cd+w_empr+'\','*.dbf')
		=copyfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_p0coem+'\',w_cd+w_empr+'\','*.fpt')
		if w_pref='E'
			private w_vfox,w_inst001,w_inst002,w_inst003,w_inst004,w_inst005,w_inst006,w_inst007,w_inst008,w_inst009,w_inst010,w_inst011,w_inst012,w_inst013,w_inst014 
			w_inst001='=Copyfile("'+w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_p0coem+'\","'+w_cd+w_empr+'\",'+'"*.bmp"'+',"'+iif(used('f77'),dbf('f77'),d0f77+'.dbf')+'")'
			w_inst002='=Copyfile("'+w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+w_p0coem+'\","'+w_cd+w_empr+'\",'+'"*.jpg"'+',"'+iif(used('f77'),dbf('f77'),d0f77+'.dbf')+'")'
			private d0f9fe
			d0f9fe=getenv('temp')+'\'+sys(3)
			w_vfox=d0f9fe+'.mem'
			save to &w_vfox. all like w_inst*
			release all like w_inst*
			=visuafox('ejeproc(.f.)',w_vfox,'Recupero de Empresa',3,1)
		endif
	else
		=copyfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\',w_cd,'*.dbf')
		=copyfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\',w_cd,'*.fpt')
	endif
endif

if mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+'Empr_Ver.dbf')
	sele 0
	use &w_cd.&w_empr.\_rkmemo\&w_sys.\Empr_Ver alias Empr_Ver
	sele Empr_Ver
	if seek(w_empr, 'F10')
		sele f10
		Replace p0vein with Empr_Ver.p0veda, p0vees with Empr_Ver.p0veda, p0veda with Empr_Ver.p0veda
	endif
	if Empr_Ver.p0memo	&& arregla f77
		if flock77()	&& abro f77
			delete for d77empr=w_empr
			pack
		endif
	endif
	use in Empr_Ver
	dele file &w_cd.&w_empr.\_rkmemo\&w_sys.\Empr_Ver.dbf
endif
						
if w_todo or w_cont=1

	if w_pref='E' 		&& empresa	
		=delefile(w_cd+w_empr+'\','*.log')
		=delefile(w_cd,'names')

		=delefile(w_cd+w_empr+'\','*.cdx')
				
		set alter to &w_cd&w_empr.\indexar.mem
		set alter on
		set conso off
		?'indexar siempre'
		set conso on
		set alter to
		set alter off

		if mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+d0f77+'.dbf')
			&& De esta manera solo levanto lo de la empresa que estoy recuperando
			if flock77()	&& abro f77
				delete for d77empr=w_empr
				append from &w_cd.&w_empr.\_rkmemo\&w_sys.\&d0f77..dbf for d77empr=w_empr
				pack
			endif
			dele file &w_cd.&w_empr.\_rkmemo\&w_sys.\&d0f77..dbf
		endif							

		if file(w_vfox)
			restore from &w_vfox. additive
			erase &w_vfox.
			if type('p_resu')='C' and len(allt(p_resu))>0
				wait wind p_resu time 2
			else
				if w_pref='E'	&& empresa
					wait wind chr(13)+chr(13)+'Termino Recupero de Empresa Exitosamente'+chr(13)+chr(13) time 2
				endif
			endif
		else
			if w_pref='E'	&& empresa
				wait wind chr(13)+chr(13)+'Termino Recupero de Empresa Exitosamente'+chr(13)+chr(13) time 2
			endif
		endif
	else
		=delefile(w_cd+w_empr+'\','*.log')
		=delefile(w_cd,'names')

		=delefile(w_cd+w_empr+'\','*.cdx')
				
		set alter to &w_cd.\indexar.mem
		set alter on
		set conso off
		?'indexar siempre'
		set conso on
		set alter to
		set alter off

		if file(w_vfox)
			restore from &w_vfox. additive
			erase &w_vfox.
			if type('p_resu')='C' and len(allt(p_resu))>0
				wait wind p_resu time 2
			else
				wait wind chr(13)+chr(13)+'Termino Recupero de Archivos de Sistema Exitosamente'+chr(13)+chr(13) time 2
			endif
		else
			wait wind chr(13)+chr(13)+'Termino Recupero de Archivos de Sistema Exitosamente'+chr(13)+chr(13) time 2
		endif
	endif
endif

if mfile(w_cd+w_empr+'\_rkmemo\'+w_sys+'\'+'ssf098.dbf')	
	private w_vfox,w_inst001,w_inst002
	w_inst001='ofs = CREATEOBJECT("scripting.filesystemobject")'
	if w_pref='E' 		&& empresa	
		w_inst002='ofs.DeleteFolder("'+w_cd+w_empr+'\_rkmemo'+'", .T.)'
	else
		w_inst002='ofs.DeleteFolder("'+w_cd+w_empr+'", .T.)'
	endif
	private d0f9fe
	d0f9fe=getenv('temp')+'\'+sys(3)
	w_vfox=d0f9fe+'.mem'
	save to &w_vfox. all like w_inst0*
	release all like w_inst0*
	=visuafox('ejeproc(.f.)',w_vfox,'Elimina la carpeta aunque no este vacia con Windows Script Host',1)
endif		
		
erase &v_fox
rele d0f9fe

wait clear

set resource on

return (w_todo or w_cont=1)

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    Procedure valid900ur
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show
PRIVATE w_retu,w_vari

w_retu=.t.
w_vari=varread()

if w_vari='W_CONT'
	if w_cont=1
		clear read
	endif
endif

if w_vari='WNCLA'
	if valclave()
		show get w_cont enable
	else
		if wcon=3
			clear read
		endif
	endif
endif

return w_retu

*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	FUNCTION VALCLAVE
*-P อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE wwcla,wok,ww,wvopc

wOk  = .f.

=multi()

wvopc=0
for ww=1 to 5
	wvopc=wvopc+iif(asc(subst(wopci,ww,1))<>32,;
				asc(subst(wopci,ww,1)),0)
next

wwcla=gencla(val(wacla),val(walet) + (wvopc + iif(!empty(whabi),asc(whabi),0))*10, '3')	

wncla= strcero(wncla)

if wncla = wwcla
   wOk=.t.
   wcon=3
else
   wOk=.f.
   Wait Wind 'N๚mero de Autorizaci๓n Incorrecto ...'
   wcon = wcon + 1
   wncla= Space(5)	
   Return(.F.)
endif

RETURN(wok)

