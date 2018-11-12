*-----------------------------------------------------------------------------
* Upgrade de Versi¢n
*-----------------------------------------------------------------------------
* Release : 21
*
w_pfile	= '999'
w_prog	= 'VERIF'
p_user	= ' '

w_title	= 'Actualización Versión '+subs(wverxexe,1,3)+'.'+subs(wverxexe,4,2)+'.'+subs(wverxexe,6,2)+' - Inconsistencias en PARAMETROS DE EMPRESA'

d0ptcv=8
d0stcv=4
wFila=3
clear
if ! mfile('\IW\fi\ivf010.dbf')
	=vMensa('No existe directorio de estructuras')
	return
endif

close data

Fieldvec[1]='F77 Semáforo'

use &d0f77 in 0 alias f77 exclusive
if neterr()
	=ErrorNet(002)
	wCont=.f.
	return
endif

wVeDa=wVersDbf

define wind winaux from 2,0 to 30,81 COLOR RGB(,,,192,192,192)

move wind winaux center	&& R.18 Mariano

activate wind winaux

wReRoot=.f.   && Se enciende cuando entra en la rutina especifica

do StruRoot
do StruEmpr
do ReplRoot


*===> Esto va acá porque en ReplEmpr se llama al ORTIVA
w_time	= TIME()
w_mensa	= ''

do ReplEmpr

=masrow(3)

=vMensa('FIN DE ACTUALIZACION')
=vMensa('Digite una tecla para Continuar',200)
=inkey(0)

clear

release window winaux

IF !EMPTY(w_mensa)
			w_resul	=	SINO("Desea ver el resultado de la verificación de datos de empresas  ?","SI", "Actualización de versión")
			IF w_resul = "S"
						IF !USED('SSF090')
										USE ssf090 IN 0 ALIAS ssf090
						ENDIF
						DO ssm998.mpr 
						DO verif2.spr
			ENDIF
ENDIF

return

*-----------------------------------------------------------------------------
proc StruRoot
*-----------------------------------------------------------------------------
* ACTUALIZACION ESTRUCTURAS DIRECTORIO \IW
* ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*set curs off

clear
@ 01,00,24,79 box

@ 01,26 say 'ACTUALIZADOR DE ESTRUCTURAS'
@ 02,02 say 'Directorio Principal: '
@ 02,25 say '\IW\'
@ 03,02 say 'Archivos:'

IF !mfile('\IW\ARCHIVOS\NUL')
			=creadir('\IW\ARCHIVOS')
ENDIF

IF !mfile('\IW\ARCHIVOS\SDIARIOS\NUL')
			=creadir('\IW\ARCHIVOS\SDIARIOS')
ENDIF

IF !mfile('\IW\AFIP\NUL')
			=creadir('\IW\AFIP')
ENDIF

&& R.03b Raúl				
IF !mfile('\IW\AFIP\GANANCIA\NUL')
			=creadir('\IW\AFIP\GANANCIA')
ENDIF
&& R.03e Raúl				

IF !mfile('\IW\AFIP\IVA\NUL')
			=creadir('\IW\AFIP\IVA')
ENDIF

IF !mfile('\IW\AFIP\SICORE\NUL')
			=creadir('\IW\AFIP\SICORE')
ENDIF

IF !mfile('\IW\AFIP\SIFERE\NUL')
			=creadir('\IW\AFIP\SIFERE')
ENDIF

IF !mfile('\IW\AFIP\CITI\NUL')
			=creadir('\IW\AFIP\CITI')
ENDIF

&& R.10b Mariano
IF !mfile('\IW\AFIP\REGINFO\NUL')
			=creadir('\IW\AFIP\REGINFO')
ENDIF
&& R.10e Mariano

&& R.12b Mariano
IF !mfile('\IW\INTERFAZ\NUL')
			=creadir('\IW\INTERFAZ')
ENDIF
&& R.12e Mariano

IF !mfile('\IW\AFIP\INGBRU\NUL')
			=creadir('\IW\AFIP\INGBRU')
ENDIF

IF !mfile('\IW\AFIP\INGBRU\CAPITAL\NUL')
			=creadir('\IW\AFIP\INGBRU\CAPITAL')
ENDIF

IF !mfile('\IW\AFIP\INGBRU\BAIRES\NUL')
			=creadir('\IW\AFIP\INGBRU\BAIRES')
ENDIF

&& R.06b Raúl
IF !mfile('\IW\AFIP\INGBRU\MENDOZA\NUL')
			=creadir('\IW\AFIP\INGBRU\MENDOZA')
ENDIF
&& R.06e Raúl

&& R.07b Mariano
IF !mfile('\IW\AFIP\INGBRU\SALTA\NUL')
			=creadir('\IW\AFIP\INGBRU\SALTA')
ENDIF
&& R.07e Mariano

declare Names[100]
wCant=adir(Names,'\IW\Fi\*.dbf')
=asort(Names)

for wFileNum=1 to wCant
	Names[wFileNum,1]=trim(Names[wFileNum,1])
	NewFile='\IW\Fi\'+alltrim(Names[wFileNum,1])
	OldFile='\IW\'+Names[wFileNum,1]

	WFILE=right(upper(substr(Names[wFileNum,1],1,LEN(Names[wFileNum,1])-4)),2)

	wFila=wFila+1

	if wfila >23
		@ 4,1 clear to 23,78
		wfila=4
	endif
	@ wFila,02 say OldFile

	* Se Borran Los Archivos de Documentacion y Guia de sistema.
	*                           Funciones y Estructuras
	if substr(OldFile,at('.',OLDFILE)-2,2)$'96.97.98.18'.or.;
			substr(OldFile,at('.',OLDFILE)-8,2)=='DC'

		if mfile(alltrim(OldFile))
			dele file &OldFile
		endif
	endif
	*
	*
	* Antes de comparar Estructura veo si hay un DBT nuevo.
	* -----------------------------------------------------
	OldDbt=strtran(OldFile,'DBF','DBT')
	NewDbt=strtran(NewFile,'DBF','DBT')
	if mfile(alltrim(NewDbt))
		if !mfile(alltrim(OldDbt))
			@ wFila,52 say 'Nuevo DBT.'
		endif
	endif

	if ! mfile(alltrim(OldFile))
		* ARCHIVO NUEVO
		* ßßßßßßßßßßßßß
		@ wFila,25 say 'Archivo Nuevo.'
		select 0
		use &NewFile
		&& R.08b Mariano
*		copy to &OldFile
		***vfp***
		if version()='Visual FoxPro'
			copy to &OldFile type fox2x
		else
			copy to &OldFile
		endif
		***vfp***
		&& R.08e Mariano
		use
		do cpzero with oldfile,1252
		
	else
		@ wFila,25 say 'Comparando Estructura.'

*===> ACTUALIZADOR NUEVO
*		if ! EqualStr(OldFile,NewFile) OR (wVeda<='409' and ('\IW\IVF'$OldFile or '\IW\FEX.DBF'=Oldfile or '\IW\FFO.DBF'=Oldfile)) && fuerzo el cambio de code-page
*		if ! EqualStr(OldFile,NewFile) OR w_rel = "A" OR (wVeda<='409' and ('\IW\IVF'$OldFile or '\IW\FEX.DBF'=Oldfile or '\IW\FFO.DBF'=Oldfile)) && fuerzo el cambio de code-page
*		if ! EqualStr(OldFile,NewFile) OR w_actver	  OR (wVeda<='409' and ('\IW\IVF'$OldFile or '\IW\FEX.DBF'=Oldfile or '\IW\FFO.DBF'=Oldfile)) && fuerzo el cambio de code-page	&& R.05 Raúl
		if ! EqualStr(OldFile,NewFile) OR (wVeda<='409' and ('\IW\IVF'$OldFile or '\IW\FEX.DBF'=Oldfile or '\IW\FFO.DBF'=Oldfile)) && fuerzo el cambio de code-page		&& R.05 Raúl

			* Hubo Cambios en Estructura
			* ßßßßßßßßßßßßßßßßßßßßßßßßßß
			@ wFila,25 say 'Actualizando Estructura.'
			sele 0
			use (NewFile) exclusive
			d0f99=sys(2023)+'\IVF'+netuser+'99'
			&& R.08b Mariano
*			copy to &d0f99
			***vfp***
			if version()='Visual FoxPro'
				copy to &d0f99 type fox2x
			else
				copy to &d0f99
			endif
			***vfp***
			&& R.08e Mariano
			use (d0f99) exclusive
			if reccount()>0
				zap
			endif
			appe from &OldFile for num()
			dele file &OldFile
			dele file &OldDbt
			&& R.08b Mariano
*			copy to &OldFile
			***vfp***
			if version()='Visual FoxPro'
				copy to &OldFile type fox2x
			else
				copy to &OldFile
			endif
			***vfp***
			&& R.08e Mariano
			zap
			use
			do cpzero with oldfile,1252
			
			@ wFila,25 say 'Estructura Actualizada. '
		else
			* Sin Cambios en Estructura
			* ßßßßßßßßßßßßßßßßßßßßßßßßß
			@ wFila,25 say 'Estructura Sin Cambios. '
		endif
	endif

next
release Names

&& R.18b Mariano
sele 0
use ssf001 alias s01
copy structure extended to &d0f99
use &d0f99 alias s01
if reccount('s01')=9
	use in s01
	dele file &d0f99..dbf
else
	w_encripta=.f.
	do case	
		case reccount('s01')=5
			w_encripta=.t.
			append blank
			replace field_name with 'D1MAIL', field_type with 'C', field_len with 100, field_dec with 0
			append blank
			replace field_name with 'D1PWEB', field_type with 'N', field_len with 1, field_dec with 0
			append blank
			replace field_name with 'D1GWEB', field_type with 'N', field_len with 1, field_dec with 0
			append blank
			replace field_name with 'D1GWSO', field_type with 'C', field_len with 10, field_dec with 0
		case reccount('s01')=8
			append blank
			replace field_name with 'D1GWSO', field_type with 'C', field_len with 10, field_dec with 0
	endcase
	use in s01
	create &d0f99.s from &d0f99
	use
	dele file &d0f99..dbf
	use &d0f99.s alias s01 
	append from ssf001
	if w_encripta	
		replace all d1pasw with encript(d1pasw,3)
	endif	&& R.86 Mariano 
	if version()='Visual FoxPro'
		w_fox2x='copy to ssf001.dbf type fox2x'
		&w_fox2x
		use in s01
		erase &d0f99.s.dbf
	else
		copy to ssf001.dbf
		use in s01
	endif
	use ssf001 exclusive
	inde on d1user tag d1user
	inde on upper(d1nomb) tag d1nomb
	use
endif
&& R.18e Mariano

return

*-----------------------------------------------------------------------------
procedure StruEmpr
*-----------------------------------------------------------------------------
* ACTUALIZACION ESTRUCTURAS SUBDIRECTORIOS
* ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
clear

* Zappeado de Archivos Nuevos
* ---------------------------

declare Names[200]
wCant=adir(Names,'\IW\Fi\00\*.dbf')
=asort(Names)
for i=1 to wCant
	Names[i,1]=alltrim(Names[i,1])
	FileName='\IW\Fi\00\'+Names[i,1]
	sele 0
	use (FileName) exclusive
	if reccount()>0
		zap
	endif
next
use

=closall()
clear

w_name='\IW\ivf010'

use &w_name alias f10 in 0 exclusive

sele f10

* CICLO EMPRESAS
* ßßßßß ßßßßßßßß
do while ! eof('f10')
	* Recorrer cada una de los subdirectorios de empresas

	wVeDa=wVersDbf              && ...

	if ! deleted('f10')
		clear
		@ 01,00,24,79 box

		@ 01,26 say 'ACTUALIZADOR DE ESTRUCTURAS'
		@ 02,02 say 'Empresa:  '
		@ 02,12 say f10->p0CoEm
		@ 02,15 say f10->p0Empr
		@ 03,02 say 'Archivos:'
		d0Path=trim(f10->p0Path)
		wFila=3

		if wVeDa<>'00000000' .and. !empty(f10->p0VeDa)      && ...
			wVeDa=f10->p0VeDa                                && ...
		endif                                               && ...

		* Checkeo y Registro de Versi¢n en f10->p0VeIn
		* --------------------------------------ßßßßßß

*===> ACTUALIZADOR NUEVO
*		if f10->p0VeEs>=wVersExe .or. wVeDa>=wVersExe						&& ...
*		if (f10->p0VeEs>=wVersExe .or. wVeDa>=wVersExe) AND w_rel = "0"     && ...
		if (f10->p0VeEs>=wVersExe .or. wVeDa>=wVersExe) AND !w_actver			&& ...

			=vMensa(' No se efectua la Actualizacion de Estructuras de esta Empresa.')
			=vMensa(' Esta registrado que ya fue realizada en otro momento.')
			=inkey(.5)
		else
&& R.02b Raúl
			IF !dispOk(f10.p0path)
					WAIT WIND 'NO EXISTE EL DIRECTORIO DE LA EMPRESA : ' + f10.p0coem + ' - ' + f10.p0empr  
					REPLA f10.p0VeIn	WITH '999', f10.p0VeEs		WITH '999'
					SKIP IN f10
					LOOP
			ENDIF
&& R.02e Raúl
			sele f10
			repl f10->p0VeIn with wVersExe
			
			w_rec10=recno('f10')
			
			for FileNum=1 to wCant
				OldFile=d0path+Names[FileNum,1]
				NewFile='\IW\FI\00\'+Names[FileNum,1]
				wFila=wFila+1
				@ wFila,02 say OldFile

				OldDbt=strtran(OldFile,'DBF','DBT')
				NewDbt=strtran(NewFile,'DBF','DBT')

				if file(alltrim(NewDbt))
					if !file(alltrim(OldDbt))
						@ wFila,52 say 'Nuevo DBT.'
					endif
				else
					if upper(NewDbt) = '\IW\FI\00\IVF008.DBT'
						@ wFila,49 say 'No Existe:'+NewDbt+''	&& R.18 Mariano (49 x 52)
					endif
				endif

				if !file(alltrim(OldFile))
					* ARCHIVO NUEVO
					* ßßßßßßßßßßßßß
					@ wFila,25 say 'Archivo Nuevo.'
					select 0
					use &NewFile
					&& R.08b Mariano
*					copy to &OldFile
					***vfp***
					if version()='Visual FoxPro'
						copy to &OldFile type fox2x
					else
						copy to &OldFile
					endif
					***vfp***
					&& R.08e Mariano
					use
					do cpzero with oldfile,1252
					
				else
					@ wFila,25 say 'Comparando Estructura.'
*===> ACTUALIZADOR NUEVO
*					if ! EqualStr(OldFile,NewFile) or wVeda<='409' 					&& fuerzo el cambio de code-page
*					if ! EqualStr(OldFile,NewFile) OR w_rel = "A" or wVeda<='409' 	&& fuerzo el cambio de code-page
*					if ! EqualStr(OldFile,NewFile) OR w_actver	  or wVeda<='409' 	&& fuerzo el cambio de code-page	&& R.05 Raúl						* Hubo Cambios en Estructura
					if ! EqualStr(OldFile,NewFile) or wVeda<='409' 					&& fuerzo el cambio de code-page	&& R.05 Raúl

						* ßßßßßßßßßßßßßßßßßßßßßßßßßß
						@ wFila,25 say 'Actualizando Estructura.'
						sele 0
						use(NewFile) exclusive
						zap
						appe from &OldFile for num()
						dele file &OldFile
						dele file &OldDbt
						&& R.08b Mariano
						***vfp***
*						copy to &OldFile
						if version()='Visual FoxPro'
							copy to &OldFile type fox2x
						else
							copy to &OldFile
						endif
						***vfp***
						&& R.08e Mariano
						zap
						use
						do cpzero with oldfile,1252

						@ wFila,25 say 'Estructura Actualizada. '
					else
						* SIN Cambios en Estructura
						* ßßßßßßßßßßßßßßßßßßßßßßßßß
						@ wFila,25 say 'Estructura Sin Cambios. '
					endif
				endif
			next
			* En cualquier Versi¢n: Registro de Versi¢n en f10->p0VeEs
			* --------------------------------------------------ßßßßßß
			sele f10
	        if between(w_rec10,1,reccount('f10'))
	         	go w_rec10 in f10
			endif
			repl f10->p0VeEs with wVersExe
		endif
	endif
	skip in f10
enddo
release Names
sele f10
use
=closall()
clear
return

*-----------------------------------------------------------------------------
proc ReplRoot
*-----------------------------------------------------------------------------
* REEMPLAZOS EN DIRECTORIO \IW
* ßßßßßßßßßßßßßßßßßßßßßßßßßßßß

wReRoot=.t.

wVeDa=wVersDbf              && ...

set curs off
clear
@ 01,00,03,79 box
@ 01,29 say 'ACTUALIZADOR DE DATOS'
@ 02,02 say 'Directorio Principal: '
@ 02,25 say '\IW\'
@ 04,00
declare Names[100]
wCant=adir(Names,'\IW\*.dbf')
=asort(Names)
for FileNum=1 to wCant
	Names[FileNum,1]=trim(Names[FileNum,1])
	wFile=Names[FileNum,1]
	if !'SSF'$wfile and !'77'$wfile
		sele 0
		use (wFile) exclusive
		wPos=at('.',Names[FileNum,1])
		wFileNNN=left(Names[FileNum,1],wPos-1)
		* ~~~
		wNfile=right(wFileNNN,2)
		d0f&wNfile='\IW\'+wFileNNN
		* ~~~~
		=Reemplazos(wFileNNN)
	endif
next
release Names

wReRoot=.f.

return

*-----------------------------------------------------------------------------
proc ReplEmpr
*-----------------------------------------------------------------------------
=closall()
clear

* REEMPLAZOS EN EMPRESAS
* ßßßßßßßßßßßßßßßßßßßßßß
w_name='\IW\ivf010'

use &w_name alias f10 in 0 exclusive

do while !eof('f10')

	wVeDa=wVersdbf

	if !deleted('f10')
		clear
		@ 01,00,03,79 box
		@ 01,29 say 'ACTUALIZADOR DE DATOS'
		@ 02,02 say 'Empresa:  '
		@ 02,12 say f10->p0CoEm
		@ 02,15 say f10->p0Empr
		@ 04,00
		if ! empty( f10->p0VeDa )    && wVeDa<>'00000000' .and.
			wVeDa=f10->p0VeDa
		endif

		* Checkeo de Versi¢n en f10->p0VeDa
		* ---------------------------ßßßßßß

*===> ACTUALIZADOR NUEVO
&& R.02b Raúl
		IF !dispOk(f10.p0path)
					WAIT WIND 'NO EXISTE EL DIRECTORIO DE LA EMPRESA : ' + f10.p0coem + ' - ' + f10.p0empr  
					REPLA f10.p0VeDa	WITH '999'
					SKIP IN f10
					LOOP
		ENDIF
&& R.02e Raúl

*		if wVeDa >= wVersExe .and. wversdbf<>'00000000' 
*		IF wveda >= wversexe  AND wversdbf<>'00000000' AND w_rel = "0"
		IF wveda >= wversexe  AND wversdbf<>'00000000' AND !w_actver
		
			=vMensa('No se efectua la Actualizacion de Datos de esta Empresa.')
			=vMensa('Esta registrado que ya fue realizada en otro momento.')
			=inkey(.5)
		else
			d0Path=trim(f10->p0Path)
			d0f00=d0path+'ivf000'
			sele 0
			
			use(d0f00)		ALIAS f00
			
			d0f00=d0path+trim(p0f00)
			d0f01=d0path+trim(p0f01)
			d0f02=d0path+trim(p0f02)
			d0f03=d0path+trim(p0f03)
			d0f04=d0path+trim(p0f04)
			d0f05=d0path+trim(p0f05)
			d0f06=d0path+trim(p0f06)
			d0f07=d0path+trim(p0f07)
			d0f08=d0path+trim(p0f08)
			d0f09=d0path+trim(p0f09)
			d0f10='\IW\IVF010'
			d0f11=d0path+trim(p0f11)
			d0f12='\IW\IVF012'
			d0f13=d0path+trim(p0f13)
			d0f14=d0path+trim(p0f14)
			d0f15=d0path+trim(p0f15)
			d0f16='\IW\IVF016'
			d0f17='\IW\IVF017'
			d0f20=d0path+trim(p0f20)
			d0f21='\IW\IVF021'
			d0f99=d0path+trim(p0f99)

			* Se guarda la empresa contable
			* -----------------------------
			d0emcg=p0emcg
			
			* Se guarda la jurisdicción AFIP
			* -----------------------------

			d0afip = p0afip
			d0juce = p0juce			
			
			* Se guarda si es Persona Fisica o Sociedad xa Ganancias
			* -----------------------------

			d0gan = p0gan

			w_retu	= .T.
			w_mensa	= ''
			
			d0coem	= f10.p0coem
			d0emp	= f10.p0empr

			DO ortiva	WITH 'ACTUALIZACION'

			use   && se cierra el ivf000


			use &d0f12 alias f12 in 0 exclusive
			sele f12
			* se abre para sacar el path contable
			* -----------------------------------
			w0fipre=''
			d0paco=trim(p0paco)

			use   && se cierra el ivf012

			if wVersdbf = '00000000'
				* Es instalacion inicial o se fuerza a que indexe todos los files
				* ---------------------------------------------------------------
				=db_indexiw(.t.,'f01')
				=db_indexiw(.t.,'f02')
				=db_indexiw(.t.,'f03')
				=db_indexiw(.t.,'f04')
				=db_indexiw(.t.,'f05')
				=db_indexiw(.t.,'f06')
				=db_indexiw(.t.,'f07')
				=db_indexiw(.t.,'f08')
				=db_indexiw(.t.,'f09')
				=db_indexiw(.t.,'f13')
				=db_indexiw(.t.,'f14')
				=db_indexiw(.t.,'f15')
				=db_indexiw(.t.,'f20')
				=db_indexiw(.t.,'f21')
			endif

			declare Names[100]
			wCant=adir(Names,d0Path+'*.dbf')
			=asort(Names)

			for FileNum=1 to wCant
				wPos=at('.',Names[FileNum,1])
				wFileNNN=left(Names[FileNum,1],wPos-1)
				wFileName=d0Path+trim(Names[FileNum,1])

				if file(alltrim(wFileName))
					sele 0
					use(wFileName) exclu
					=Reemplazos(wFileNNN)
				else
					=vMensa('No existe archivo '+wFileName,200)
				endif

			next

			* Registro de Versi¢n en f10->p0VeDa
			* ----------------------------ßßßßßß
			sele f10
			repl f10->p0VeDa with wVersExe
		endif
	endif
	skip in f10
enddo
release Names
sele f10
use
=closall()
return

*-----------------------------------------------------------------------------
function Reemplazos
*-----------------------------------------------------------------------------
para wArchivo

* Un IF para cada Versi¢n, y allí un CASE para cada File.

go top
* ACTUALIZACION DE DATOS DE LA VERSION 4.00
* -----------------------------------------
if wVeDa <= '400'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF001'
			=vMensa('Version 4.00: Alicuota IVA',200)
			=masrow(1)
			do while !eof()
				if empty(d1alic) and trim(d1tip)$'RV.RC'
					repl d1alic with '003'	&& iva=21.00 acrec=10.50
				endif
				skip
			enddo

		case wArchivo='IVF004'
			=vMensa('Version 4.00: Periodo en 04',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d4peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d4peri),3)+'19'+right(alltrim(d4peri),2)
					=vMensa('Anterior : '+d4peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d4peri with wperi
				endif
				if val(left(alltrim(d4asve),2))>20
					*Si viene el anio con 2 digitos asume el milenio 1900
					wasve='19'+left(alltrim(d4asve),len(alltrim(d4asve))-2)+'19'+right(alltrim(d4asve),2)
					=vMensa('Anterior : '+d4asve)
					=masrow(1)
					=vMensa('Nuevo    : '+wasve)
					repl d4asve with wasve
				endif
				if val(left(alltrim(d4asco),2))>20
					*Si viene el anio con 2 digitos asume el milenio 1900
					wasco='19'+left(alltrim(d4asco),len(alltrim(d4asco))-2)+'19'+right(alltrim(d4asco),2)
					=vMensa('Anterior : '+d4asco)
					=masrow(1)
					=vMensa('Nuevo    : '+wasco)
					repl d4asco with wasco
				endif
				skip
			enddo

		case wArchivo='IVF005'
			=vMensa('Version 4.00: Periodo y Per/Ret/Otros en 05',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d5peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d5peri),3)+'19'+right(alltrim(d5peri),2)
					=vMensa('Anterior : '+d5peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d5peri with wperi
				endif
				if empty(d5pere)
					repl d5pere with '000'
				endif
				skip
			enddo

		case wArchivo='IVF006'
			=vMensa('Version 4.00: Periodo en 06',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d6peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d6peri),3)+'19'+right(alltrim(d6peri),2)
					=vMensa('Anterior : '+d6peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d6peri with wperi
				endif
				skip
			enddo

		case wArchivo='IVF007'
			=vMensa('Version 4.00: Periodo en 07',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d7peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d7peri),3)+'19'+right(alltrim(d7peri),2)
					=vMensa('Anterior : '+d7peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d7peri with wperi
				endif
				skip
			enddo

		case wArchivo='IVF013'
			=vMensa('Version 4.00: Periodo en 13',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d13peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d13peri),3)+'19'+right(alltrim(d13peri),2)
					=vMensa('Anterior : '+d13peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d13peri with wperi
				endif
				skip
			enddo

		case wArchivo='IVF015'
			=vMensa('Version 4.00: Periodo en 15',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d15peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d15peri),3)+'19'+right(alltrim(d15peri),2)
					=vMensa('Anterior : '+d15peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d15peri with wperi
				endif
				skip
			enddo

		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.00: Reemplazo Funcion SI() y IF() por IIF() 17',200)
			=masrow(1)
			
			go top
			do while !eof()
				if 'SI('$upper(d17form)
					=vMensa('Titulo : '+d17titu)
					=masrow(1)
					replace d17form with strtran(upper(d17form), 'SI(', 'IIF(')
				endif
				if 'SI '$upper(d17form)
					=vMensa('Titulo : '+d17titu)
					=masrow(1)
					replace d17form with strtran(upper(d17form), 'SI ', 'IIF ')
				endif
				if 'IF('$upper(d17form) and !'IIF('$upper(d17form)
					=vMensa('Titulo : '+d17titu)
					=masrow(1)
					replace d17form with strtran(upper(d17form), 'IF(', 'IIF(')
				endif
				if 'IF '$upper(d17form) and !'IIF '$upper(d17form)
					=vMensa('Titulo : '+d17titu)
					=masrow(1)
					replace d17form with strtran(upper(d17form), 'IF ', 'IIF ')
				endif

				if empty(d17dpro)
					repl d17dpro with '000'
				endif

				if empty(d17hpro)
					repl d17hpro with '999'
				endif

				if empty(d17dali)
					repl d17dali with '001'
				endif

				if empty(d17hali)
					repl d17hali with '004'
				endif

				if 'TMC'$upper(d17form) .or.;
						'REN'$upper(d17form) .or.;
						'SRE'$upper(d17form)
					* Para saltear los renglones que calculan en tiempo de
					* Totales
					skip
					loop
				endif

				if empty(d17prot)
					repl d17prot with 'P/R/O'
				endif

				if empty(d17clas)
					repl d17clas with '1/2/3/A/B/C/D/E/F/G'
				endif

				if alltrim(D17COMP)=='FA/DB/CR/TI/RE/FC/RF/LV'
					repl D17COMP with alltrim(D17COMP)+'/TF'
				endif
				
				skip
			enddo

		case wArchivo='IVF020'
			=vMensa('Version 4.00: Periodo en 20',200)
			=masrow(1)
			do while !eof()
				if len(alltrim(d20peri))=5
					*Si viene el anio con 2 digitos asume el milenio 1900
					wperi=left(alltrim(d20peri),3)+'19'+right(alltrim(d20peri),2)
					=vMensa('Anterior : '+d20peri)
					=masrow(1)
					=vMensa('Nuevo    : '+wperi)
					repl d20peri with wperi
				endif
				skip
			enddo

		case wArchivo='FEX' and wReRoot
			=vMensa('Version 4.00: Reemplazo Funcion SI() y IF() por IIF() FEX',200)
			=masrow(1)

			do while !eof()
				if 'SI('$upper(d44expr)
					=vMensa('Titulo : '+d44nomb)
					=masrow(1)
					replace d44expr with strtran(upper(d44expr), 'SI(', 'IIF(')
				endif
				if 'SI '$upper(d44expr)
					=vMensa('Titulo : '+d44nomb)
					=masrow(1)
					replace d44expr with strtran(upper(d44expr), 'SI ', 'IIF ')
				endif
				if 'IF('$upper(d44expr) and !'IIF('$upper(d44expr)
					=vMensa('Titulo : '+d44nomb)
					=masrow(1)
					replace d44expr with strtran(upper(d44expr), 'IF(', 'IIF(')
				endif
				if 'IF '$upper(d44expr) and !'IIF '$upper(d44expr)
					=vMensa('Titulo : '+d44nomb)
					=masrow(1)
					replace d44expr with strtran(upper(d44expr), 'IF ', 'IIF ')
				endif

				skip
			enddo

	endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.01
* -----------------------------------------
if wVeDa <= '401'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
        case wArchivo='IVF017'.and. wReRoot

             =vMensa('Version 4.01: Informes Definidos 17',200)
             =masrow(1)

             repl for alltrim(d17clas)='1/2/3/4/5/6/7/8' d17clas with '1/2/3/A/B/C/D/E/F/G'

*		case wArchivo='IVF023' and wReRoot
*			=vMensa('Version 4.01: Alicuota Exenta en 23',200)
*         =masrow(1)

*         go top
*         loca for d23codi=='000'
*         if !found()
*            appe blan
*            repl d23codi with '000',;
*                 d23desc with 'ALICUOTA  0.00 %  Y   0.00 % '
*         endif

   endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.02
* -----------------------------------------
if wVeDa <= '402'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF023' and wReRoot
			=vMensa('Version 4.02: Alicuota nueva en 23',200)
         =masrow(1)

         loca for d23codi=='005'
         if !found()
            appe blan
            repl d23codi with '005',;
                 d23desc with 'ALICUOTA 13.00 %  Y   6.50 %',;
                 d23alic with 13.00,;
                 d23acre with  6.50
         endif

   endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.03
* -----------------------------------------
if wVeDa <= '403'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF000'
			=vMensa('Version 4.03: Cod.postal c(8) y leyenda Otras Ventas c(6) en f00',200)
         =masrow(1)

         replace p0leot with 'Otros'

   endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.05
* -----------------------------------------
if wVeDa <= '405'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.05: Otros comprobantes en 17',200)
			=masrow(1)
			
			go top
			do while !eof()
				if alltrim(D17COMP)=='FA/DB/CR/TI/RE/FC/RF/LV/TF'
					repl D17COMP with alltrim(D17COMP)+'/OT'
				endif
				
				skip
			enddo
	endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.06
* -----------------------------------------
if wVeDa <= '406'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF001'
*			=vMensa('Version 4.06: Alicuota IVA',200)
*			=masrow(1)
*			do while !eof()
*				if trim(d1tip)$'RV.RC'
*					do case
*						case d1alic='001'
*							repl d1alic with '006'	&& iva=10.50 a iva=9.50
*						case d1alic='002'
*							repl d1alic with '007'	&& iva=10.50 a iva=9.50
*						case d1alic='003'
*							repl d1alic with '008'	&& iva=21.00 a iva=19.00
*					endcase
*				endif
*				skip
*			enddo
			** los modifico luego de modificar ivf005 ** bug

		case wArchivo='IVF005'
			=db_indexiw(.t.,'f01')		&& indexo antes de abrir por las dudas que no haya indice
			use &d0f01 in 0 order d1tip alias f01_1 
			use &d0f01 in 0 order d1tip alias f01_2 again
			sele ivf005
			set rela to 'RV'+d5rubr into f01_1
			set rela to 'RC'+d5rubr into f01_2 additive

			=vMensa('Version 4.06: Alicuotas de Iva en 05',200)
			=masrow(1)
			repl all d5alic with iif(d5tipo='V', f01_1.d1alic, f01_2.d1alic)
			
			use in f01_1
			
			** arreglo luego ivf001 desde aqui, ya que sino grabo mal el ivf005 ** bug
			=set_alia('f01_2')
			go top
			do while !eof()
				if trim(d1tip)$'RV.RC'
					do case
						case d1alic='001'
							repl d1alic with '006'	&& iva=10.50 a iva=9.50
						case d1alic='002'
							repl d1alic with '007'	&& iva=10.50 a iva=9.50
						case d1alic='003'
							repl d1alic with '008'	&& iva=21.00 a iva=19.00
					endcase
				endif
				skip
			enddo
			=rest_alia()
			
			use in f01_2
			
		case wArchivo='IVF016' and wReRoot
			=vMensa('Version 4.06: Definiciones nuevas en 16',200)
			=masrow(1)
			
			locate for d16defi='108'
			if !found()		&& sino existe la tomo
				append from fi\ivf016 for d16defi='108'	&& tomo todos los registros de la definicion
			endif
			locate for d16defi='110'
			if !found()		&& sino existe la tomo
				append from fi\ivf016 for d16defi='110'	&& tomo todos los registros de la definicion
			endif
			locate for d16defi='112'
			if !found()		&& sino existe la tomo
				append from fi\ivf016 for d16defi='112'	&& tomo todos los registros de la definicion
			endif

		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.06: Definiciones nuevas en 17',200)
			=masrow(1)
			
			go top
			do while !eof()
				if alltrim(D17COMP)=='FA/DB/  /TI/RE/FC/RF/LV/TF'
					repl D17COMP with alltrim(D17COMP)+'/OT'
				endif
				
				skip
			enddo

			locate for d17defi='108'
			if !found()		&& sino existe la tomo
				append from fi\ivf017 for d17defi='108'	&& tomo todos los registros de la definicion
			endif
			locate for d17defi='110'
			if !found()		&& sino existe la tomo
				append from fi\ivf017 for d17defi='110'	&& tomo todos los registros de la definicion
			endif
			locate for d17defi='112'
			if !found()		&& sino existe la tomo
				append from fi\ivf017 for d17defi='112'	&& tomo todos los registros de la definicion
			endif

		case wArchivo='IVF023' and wReRoot
			=vMensa('Version 4.06: Alicuotas nuevas en 23',200)
         =masrow(1)

         loca for d23codi=='006'
         if !found()
            appe blan
            repl d23codi with '006',;
                 d23desc with 'ALICUOTA  9.50 %  Y   0.00 %',;
                 d23alic with 9.50,;
                 d23acre with 0
            appe blan
            repl d23codi with '007',;
                 d23desc with 'ALICUOTA  9.50 %  Y   4.75 %',;
                 d23alic with 9.50,;
                 d23acre with 4.75
            appe blan
            repl d23codi with '008',;
                 d23desc with 'ALICUOTA 19.00 %  Y   9.50 %',;
                 d23alic with 19.00,;
                 d23acre with 9.50
         endif
         
         repl all d23fusa with .t.

   endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.07
* -----------------------------------------
if wVeDa <= '407'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF005'

			=vMensa('Version 4.07: Alicuotas de Iva en 05',200)
			=masrow(1)
			** arregla el bug de version anterior **
			repl d5alic with '003' for d5alic='008' and dtos(d5fech)<='20021117'
			repl d5alic with '002' for d5alic='007' and dtos(d5fech)<='20021117'
			repl d5alic with '001' for d5alic='006' and dtos(d5fech)<='20021117'


   endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.08
* -----------------------------------------
if wVeDa <= '408'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF005'

			=vMensa('Version 4.08: Alicuotas de Iva en 05',200)
			=masrow(1)
			** arregla alicuotas si figuran mal de version anterior **
			repl d5alic with '008' for d5alic='003' and between(dtos(d5fech),'20021118','20030117')
			repl d5alic with '007' for d5alic='002' and between(dtos(d5fech),'20021118','20030117')
			repl d5alic with '006' for d5alic='001' and between(dtos(d5fech),'20021118','20030117')

			** arregla alicuotas si figuran mal de version anterior **
			repl d5alic with '003' for d5alic='008' and dtos(d5fech)>='20030118'
			repl d5alic with '002' for d5alic='007' and dtos(d5fech)>='20030118'
			repl d5alic with '001' for d5alic='006' and dtos(d5fech)>='20030118'

			=db_indexiw(.t.,'f01')		&& indexo antes de abrir por las dudas que no haya indice
			use &d0f01 in 0 order d1tip alias f01
			sele ivf005
			** arreglo ivf001 desde aqui, ya que sino grabo mal el ivf005
			=set_alia('f01')
			go top
			do while !eof()
				if trim(d1tip)$'RV.RC'
					do case
						case d1alic='006'
							repl d1alic with '001'	&& iva=9.50 a iva=10.50
						case d1alic='007'
							repl d1alic with '002'	&& iva=9.50 a iva=10.50
						case d1alic='008'
							repl d1alic with '003'	&& iva=19.00 a iva=21.00
					endcase
				endif
				skip
			enddo
			=rest_alia()
			
			use in f01
			
   endcase
endif


* ACTUALIZACION DE DATOS DE LA VERSION 4.09
* -----------------------------------------

if wVeDa <= '409'	&& R.19 Mariano (Puse <= en lugar de <)
	do case

		case wArchivo='IVF000'

			=vMensa('Version 4.09: Cambio codpage en 00',200)
			=masrow(1)
			if cpdbf()=1252 and .f.
				replace p0emp with cpconvert(1252,850,p0emp) all
				replace p0dir with cpconvert(1252,850,p0dir) all
				replace p0loc with cpconvert(1252,850,p0loc) all
				replace p0tlp with cpconvert(1252,850,p0tlp) all
				replace p0sys with cpconvert(1252,850,p0sys) all
				replace p0ocv1 with cpconvert(1252,850,p0ocv1) all
				replace p0ocv2 with cpconvert(1252,850,p0ocv2) all
				replace p0ocv3 with cpconvert(1252,850,p0ocv3) all
				replace p0occ1 with cpconvert(1252,850,p0occ1) all
				replace p0occ2 with cpconvert(1252,850,p0occ2) all
				replace p0occ3 with cpconvert(1252,850,p0occ3) all
				replace p0leot with cpconvert(1252,850,p0leot) all

			endif


		case wArchivo='IVF005'

			=vMensa('Version 4.09: Cambio codpage en 05',200)
			=masrow(1)
			if cpdbf()=1252 and .f.
				replace d5leye with cpconvert(1252,850,d5leye) all
			endif

		case wArchivo='IVF012'

			=vMensa('Version 4.09: Cambio codpage en 12',200)
			=masrow(1)
			if cpdbf()=1252 and .f.
				replace p0dici with cpconvert(1252,850,p0dici) all
			endif

		case wArchivo='IVF017'

			=vMensa('Version 4.09: Cambio codpage en 17',200)
			=masrow(1)
			if cpdbf()=1252 and .f.
				replace d17titu with cpconvert(1252,850,d17titu) all
				replace d17form with cpconvert(1252,850,d17form) all
				replace d17come with cpconvert(1252,850,d17come) all
				
			endif

		case wArchivo='IVF023'

			=vMensa('Version 4.09: Cambio codpage en 23',200)
			=masrow(1)
			if cpdbf()=1252 and .f.
				replace d23desc with cpconvert(1252,850,d23desc) all
			endif
   endcase

	if cpdbf()=1252	and .f.	&& cambio el codepage del archivo
		go top
		private w_file
		w_file=dbf()
		do cpzero with w_file,850
	endif	

endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.10
* -----------------------------------------
if wVeDa <= '410'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF000'
			=masrow(1)

		case wArchivo='IVF003'
			=masrow(1)
			replace d3icai with 'N' for empty(d3icai)
			
		case wArchivo='IVF014'
			=masrow(1)
			replace d14icai with 'N' for empty(d14icai)
			replace d14imfi with 'N' for empty(d14imfi)
			
		case wArchivo='IVF024' and wReRoot
			=vMensa('Version 4.10: Percep./ret. de IVA en 24',200)
			=masrow(1)
			replace d24tipo with 'I' for empty(d24tipo)

		case wArchivo='IVF025' and wReRoot
			=vMensa('Version 4.10: Normas en 25',200)
			=masrow(1)

			if reccount()=0
				append from fi\ivf025	&& tomo todos los registros
			endif
	endcase
endif

* ACTUALIZACION DE DATOS DE LA VERSION 4.11
* -----------------------------------------
if wVeDa <= '411'	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF000'
			=masrow(1)
			replace p0cciva with 'S' for empty(p0cciva) and p0emcg='99'
			replace p0cciva with 'N' for empty(p0cciva)
*						IF EMPTY(p0gan) OR (p0gan <> "P" AND p0gan <> "S")			&& R.04 Raúl
						IF EMPTY(p0gan) OR (p0gan <> "F" AND p0gan <> "J")			&& R.04 Raúl
										w_gan = .T.
						ENDIF
						IF EMPTY(d0juce)
									=vMensa('Version 4.11: Valida Jurisdicción en 00',200)
									=masrow(1)
									w_juce = .T.
						ELSE
							use &d0f01 in 0 alias f01
							SELE f01 
							LOCATE FOR d1tip == 'JU' AND ALLT(d1cla) == ALLT(d0juce)
							IF !FOUND()
									w_juce = .T.
*									USE						&& R.04 Raúl
							ELSE
								IF EMPTY(d0afip)

									&& R.01b Mariano
									Sele f01
									locate for d1tip='JU' and empty(d1tdgi)
									if empty(d1tdgi) and !eof()
										=vMensa('Version 4.11: Actualiza JURISDICCION AFIP en 01',200)
										=masrow(1)
										=creatipos("V")
										SELE f01
										do while d1tip='JU' and !eof()
											if empty(d1tdgi)
												sele f01t
												locate for f01t.tipo='JD' and upper(right(allt(f01.d1des),5))=upper(right(allt(f01t.desc),5))
												IF found()
													SELE f01
													REPLA d1tdgi    WITH f01t.clav
												else
													locate for f01t.tipo='JD' and upper(left(f01.d1des,5))=upper(left(f01t.desc,5))
													IF found()
														SELE f01
														REPLA d1tdgi    WITH f01t.clav
													endif
												ENDIF
											endif
											SELE f01
											skip
											do while (d1tip!='JU' or !empty(d1tdgi)) and !eof()
												skip
											enddo
										enddo
										=borratipos()
									endif
									&& R.01e Mariano
									
									=vMensa('Version 4.11: Actualiza JURISDICCION AFIP en 00',200)
									=masrow(1)
									=creatipos("V")
									SELE f01 
									IF SEEK('JD' + f01.d1tdgi,'f01t')
													SELE &warchivo
													REPLA p0afip    WITH f01t.clav
*													USE		&& R.01 Mariano
									ELSE
										w_juri = .T.
									ENDIF
*									SELE f01	&& R.01 Mariano
*									USE			&& R.01 Mariano
									=borratipos()
								ENDIF
							ENDIF
							SELE f01	&& R.01 Mariano
							USE			&& R.01 Mariano
						ENDIF								
						SELE &warchivo	&& R.01 Mariano

		case wArchivo='IVF001'
			delete for d1tip = 'PR' and d1cla = 'P'	&& elimino percep/ret IVA.
			delete for d1tip = 'PR' and d1cla = 'R'	&& elimino percep/ret IVA.
			delete for d1tip = 'PG' and d1cla = 'P'	&& elimino percep/ret GAN.
			delete for d1tip = 'PG' and d1cla = 'R'	&& elimino percep/ret GAN.
			delete for d1tip = 'OT' and d1cla = 'P'	&& elimino percep/ret OTROS.
			delete for d1tip = 'OT' and d1cla = 'R'	&& elimino percep/ret OTROS.
			delete for d1tip = 'PR' and d1cla = 'O'	&& elimino percep/ret otros.

			&& R.01b Mariano
			locate for d1tip='JU' and empty(d1tdgi)
			if empty(d1tdgi) and !eof()
				=vMensa('Version 4.11: Actualiza JURISDICCION AFIP en 01',200)
				=masrow(1)
				=creatipos("V")
				SELE &warchivo
				do while d1tip='JU' and !eof()
					if empty(d1tdgi)
						sele f01t
						locate for f01t.tipo='JD' and upper(right(allt(ivf001.d1des),5))=upper(right(allt(f01t.desc),5))
						IF found()
							SELE &warchivo
							REPLA d1tdgi    WITH f01t.clav
						else
							locate for f01t.tipo='JD' and upper(left(ivf001.d1des,5))=upper(left(f01t.desc,5))
							IF found()
								SELE &warchivo
								REPLA d1tdgi    WITH f01t.clav
							endif
						ENDIF
					endif
					SELE &warchivo
					skip
					do while (d1tip!='JU' or !empty(d1tdgi)) and !eof()
						skip
					enddo
				enddo
				=borratipos()
			endif
			SELE &warchivo
			&& R.01e Mariano

		CASE wArchivo='IVF005'
			=vMensa('Version 4.11: Actualiza campos en 05',200)
			=masrow(1)
			REPLACE ALL d5prco		WITH " "		FOR (d5prco = "P" AND d5regi = "P")
			REPLACE ALL d5pere		WITH " "		FOR d5pere = "000"
			REPLACE ALL d5tiop		WITH "E",									;
						d5tifo		WITH " ",									;
						d5titr		WITH "N"		FOR d5tico = "R"

		CASE wArchivo='IVF012' and wReRoot
			=vMensa('Version 4.11: Actualiza campos en 12',200)
			=masrow(1)
			REPLACE ALL p0dici		WITH "\IW\AFIP\CITI" for empty(p0dici)	&& R.10 Mariano (agregue for empty...)

		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.11: Actualiza campos en 17',200)
			=masrow(1)
			replace all d17prot 	with strtran(d17prot,'O','')										&& elimino otros, no se usa
			replace 	d17dpro		with '' 								for d17dpro  = '000'		&& ponemos nulo para que tome todos los casos, ya que puede no tener datos
			REPLACE ALL d17form		WITH "IVAIN+IVANI"						FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "072"
			REPLACE ALL d17filt		WITH "FORMU <> 'I' "					FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "072"
			REPLACE ALL d17clas		WITH "1/3/D/E/G/ / / / / "				FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "072"
			REPLACE ALL d17form		WITH "IMPRE"							FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "112"
			REPLACE ALL d17filt		WITH " f05.d5reiv = 'R' "				FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "112"
			REPLACE ALL d17comp		WITH "FA/DB/CR/TI/RE/FC/RF/LV/TF/OT/RT"	FOR "100" <= d17defi AND d17defi <= "112" AND d17orde = "112"

		CASE wArchivo='IVF021' and wReRoot
							=vMensa('Version 4.11: Actualiza campos en 21',200)
							=masrow(1)
							USE \IW\FI\IVF021 IN 0		ALIAS f21
							SELE f21
							GO TOP
							DO WHILE !EOF()
										SCATTER MEMVAR
										SELE IVF021
										LOCATE FOR d21tip == f21.d21tip AND d21cla == f21.d21cla
										IF !FOUND()
												APPE BLANK
												GATHER MEMVAR
										ENDIF
										SELE f21
										SKIP
							ENDDO
							USE
							SELE IVF021							

		CASE wArchivo='IVF024' and wReRoot
							=vMensa('Version 4.11: Actualiza campos en 24',200)
							=masrow(1)
							USE \IW\FI\IVF024 IN 0		ALIAS f24
							SELE f24
							GO TOP
							DO WHILE !EOF()
										SCATTER MEMVAR
										SELE IVF024
										LOCATE FOR d24tipo == f24.d24tipo AND d24codi == f24.d24codi
										IF !FOUND()
												APPE BLANK
												GATHER MEMVAR
										ENDIF
										SELE f24
										SKIP
							ENDDO
							USE
							SELE IVF024
														
		CASE wArchivo='IVF025' and wReRoot
							=vMensa('Version 4.11: Actualiza campos en 25',200)
							=masrow(1)
							USE FI\IVF025 IN 0		ALIAS f25
							SELE f25
							GO TOP
							DO WHILE !EOF()
										SCATTER MEMVAR
										SELE IVF025
										LOCATE FOR d25tdgi == f25.d25tdgi AND d25refe == f25.d25refe	AND d25cafi == f25.d25cafi	AND d25codi == f25.d25codi
										IF !FOUND()
												APPE BLANK
												GATHER MEMVAR
										ENDIF
										SELE f25
										SKIP
							ENDDO
							USE
							SELE IVF025
	endcase
endif

IF wVeDa <= '412'	&& R.19 Mariano (Puse <= en lugar de <)
	DO CASE
		CASE wArchivo = 'IVF005'
						=vMensa('Version 4.12: Actualiza campos en 05',200)
						=masrow(1)
						USE IVF023 IN 0		ALIAS f23
						SELE f23
						GO TOP
						DO WHILE !EOF()
									SELE IVF005
									REPLA ALL d5por1 WITH f23.d23alic	FOR !EMPTY(d5por1) AND d5alic = f23.d23codi
									SELE f23
									SKIP
						ENDDO
						USE
						SELE IVF005

		CASE wArchivo = 'IVF010' and wReRoot	&& R.07 Mariano (Agregue wreroot)
						=vMensa('Version 4.12: Actualiza campos en 10',200)
						=masrow(1)
						
						REPLA ALL p0vant	WITH wVersDbf
						REPLA ALL p0aviso	WITH 'S'	FOR EMPTY(p0aviso)
						GO TOP
						DO WHILE !EOF()
&& R.02b Raúl
			IF !dispOk(ivf010.p0path)
					SKIP IN ivf010
					LOOP
			ENDIF
&& R.02e Raúl
									IF EMPTY(p0peri)
											w__path = ALLT(ivf010.p0path) + 'IVF004'
											USE &w__path. IN 0		ALIAS f04
											SELE f04
											INDEX ON PTOI(d4peri)	TO pepe
											GO BOTTOM
											SELE ivf010
											REPLA p0peri		WITH f04.d4peri		
											SELE f04
											USE
											DELE FILE pepe.idx
									ENDIF
									SELE IVF010
									SKIP
						ENDDO
	ENDCASE
ENDIF

*===> ACTUALIZADOR NUEVO 

IF 	wveda <= '412'	&& R.07 Mariano (saque  or (wveda='412' and w_actver) )	&& R.19 Mariano (Puse <= en lugar de <)

	DO CASE

		case wArchivo='IVF000'
			&& R.01b Mariano

			=masrow(1)
			replace p0cciva with 'S' for empty(p0cciva) and p0emcg='99'
			replace p0cciva with 'N' for empty(p0cciva)
*						IF EMPTY(p0gan) OR (p0gan <> "P" AND p0gan <> "S")			&& R.04 Raúl
						IF EMPTY(p0gan) OR (p0gan <> "F" AND p0gan <> "J")			&& R.04 Raúl
									w_gan = .T.
						ENDIF
						IF EMPTY(d0juce)
									=vMensa('Valida Jurisdicción en 00',200)
									=masrow(1)
									w_juce = .T.
						ELSE
							use &d0f01 in 0 alias f01
							SELE f01 
							LOCATE FOR d1tip == 'JU' AND ALLT(d1cla) == ALLT(d0juce)
							IF !FOUND()
									w_juce = .T.
*									USE					&& R.04 Raúl
							ELSE
								IF EMPTY(d0afip)
									Sele f01
									locate for d1tip='JU' and empty(d1tdgi)
									if empty(d1tdgi) and !eof()
										=vMensa('Actualiza JURISDICCION AFIP en 01',200)
										=masrow(1)
										=creatipos("V")
										SELE f01
										do while d1tip='JU' and !eof()
											if empty(d1tdgi)
												sele f01t
												locate for f01t.tipo='JD' and upper(right(allt(f01.d1des),5))=upper(right(allt(f01t.desc),5))
												IF found()
													SELE f01
													REPLA d1tdgi    WITH f01t.clav
												else
													locate for f01t.tipo='JD' and upper(left(f01.d1des,5))=upper(left(f01t.desc,5))
													IF found()
														SELE f01
														REPLA d1tdgi    WITH f01t.clav
													endif
												ENDIF
											endif
											SELE f01
											skip
											do while (d1tip!='JU' or !empty(d1tdgi)) and !eof()
												skip
											enddo
										enddo
										=borratipos()
									endif
									
									=vMensa('Actualiza JURISDICCION AFIP en 00',200)
									=masrow(1)
									=creatipos("V")
									SELE f01 
									IF SEEK('JD' + f01.d1tdgi,'f01t')
													SELE &warchivo
													REPLA p0afip    WITH f01t.clav
													USE
									ELSE
										w_juri = .T.
									ENDIF
									=borratipos()
								ENDIF
							ENDIF
							SELE f01
							USE
						ENDIF								
						SELE &warchivo
			&& R.01e Mariano


		case wArchivo='IVF001'
			&& R.01b Mariano

			delete for d1tip = 'PR' and d1cla = 'P'	&& elimino percep/ret IVA.
			delete for d1tip = 'PR' and d1cla = 'R'	&& elimino percep/ret IVA.
			delete for d1tip = 'PG' and d1cla = 'P'	&& elimino percep/ret GAN.
			delete for d1tip = 'PG' and d1cla = 'R'	&& elimino percep/ret GAN.
			delete for d1tip = 'OT' and d1cla = 'P'	&& elimino percep/ret OTROS.
			delete for d1tip = 'OT' and d1cla = 'R'	&& elimino percep/ret OTROS.
			delete for d1tip = 'PR' and d1cla = 'O'	&& elimino percep/ret otros.

			locate for d1tip='JU' and empty(d1tdgi)
			if empty(d1tdgi) and !eof()
				=vMensa('Version 4.11: Actualiza JURISDICCION AFIP en 01',200)
				=masrow(1)
				=creatipos("V")
				SELE &warchivo
				do while d1tip='JU' and !eof()
					if empty(d1tdgi)
						sele f01t
						locate for f01t.tipo='JD' and upper(right(allt(ivf001.d1des),5))=upper(right(allt(f01t.desc),5))
						IF found()
							SELE &warchivo
							REPLA d1tdgi    WITH f01t.clav
						else
							locate for f01t.tipo='JD' and upper(left(ivf001.d1des,5))=upper(left(f01t.desc,5))
							IF found()
								SELE &warchivo
								REPLA d1tdgi    WITH f01t.clav
							endif
						ENDIF
					endif
					SELE &warchivo
					skip
					do while (d1tip!='JU' or !empty(d1tdgi)) and !eof()
						skip
					enddo
				enddo
				=borratipos()
			endif
			SELE &warchivo
			&& R.01e Mariano


	ENDCASE
ENDIF


IF 	wveda <= '413' 	&& R.09 Mariano  (or (wveda='413' and w_actver))	&& R.19 Mariano (Puse <= en lugar de <)

&& R.06b Raúl
	DO CASE
		case wArchivo='IVF000'
			=vMensa('Actualiza DATOS SUBDIARIOS en 00',200)
			=masrow(1)
			GO TOP
			IF EMPTY(p0lin1)
					REPLA p0lin1 WITH p0emp
			ENDIF
			IF EMPTY(p0lin2)
					REPLA p0lin2 WITH 'SISTEMA DE IVA' 
			ENDIF
			IF EMPTY(p0lin3)
					REPLA p0lin3 WITH ''
			ENDIF
			IF EMPTY(p0tit1)
					REPLA p0tit1 WITH 'SUBDIARIO IVA VENTAS'
			ENDIF
			IF EMPTY(p0subt1)
					REPLA p0subt1 WITH ''
			ENDIF
			IF EMPTY(p0tit2)
					REPLA p0tit2 WITH 'SUBDIARIO IVA COMPRAS'
			ENDIF
			IF EMPTY(p0subt2)
					REPLA p0subt2 WITH ''
			ENDIF
			IF EMPTY(p0lintxt)
					REPLA p0lintxt WITH 'S'
			ENDIF
			IF EMPTY(p0peri)
					REPLA p0peri WITH 'S'
			ENDIF
			IF EMPTY(p0linperi)
					REPLA p0linperi WITH '6'
			ENDIF
			IF EMPTY(p0fecha)
					REPLA p0fecha WITH 'S'
			ENDIF
			IF EMPTY(p0hoja)
					REPLA p0hoja WITH 'S'
			ENDIF
			IF EMPTY(p0txthoja)
					REPLA p0txthoja WITH ' Hoja :'
			ENDIF
&& R.06e Raúl
	ENDCASE
ENDIF

* ACTUALIZACION DE DATOS DE LA VERSION 4.14
* -----------------------------------------
&& R.09b Mariano
if wVeDa <= '414' 	&&or (wveda='414' and w_actver)	&& R.10 Mariano	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF023' and wReRoot
			=vMensa('Version 4.14: Alicuota 2.5% y 5% en 23',200)
         =masrow(1)

         go top
         loca for d23codi=='009'
         if !found()
            appe blan
            repl d23codi with '009',;
                 d23desc with 'ALICUOTA  2.50 %  Y   0.00 % '
         endif
         loca for d23codi=='010'
         if !found()
            appe blan
            repl d23codi with '010',;
                 d23desc with 'ALICUOTA  5.00 %  Y   0.00 % '
         endif
         loca for d23codi=='011'
         if !found()
            appe blan
            repl d23codi with '011',;
                 d23desc with 'ALICUOTA  21.00 %  Y   0.00 % '
         endif
   endcase
endif
&& R.09e Mariano


* ACTUALIZACION DE DATOS DE LA VERSION 4.15
* -----------------------------------------
&& R.10b Mariano
if wVeDa <= '415' 	&& or (wveda='415' and w_actver)	&& R.11 Mariano		&& R.19 Mariano (Puse <= en lugar de <)
	do case
		case wArchivo='IVF005'
			=vMensa('Version 4.15: Codigo Comprobante Afip en 05',200)
	         =masrow(1)

    		repla d5coco with '0'+d5coco for len(allt(d5coco))=2 and len(d5coco)=3
    		repla d5tiop with '0' for d5tiop='O'

		CASE wArchivo='IVF012' and wReRoot
			=vMensa('Version 4.11: Actualiza campos en 12',200)
			=masrow(1)
			REPLACE p0diri		WITH "\IW\AFIP\REGINFO" for empty(p0diri)

		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.15: Actualiza comprobantes en 17',200)
			=masrow(1)
			REPLACE d17comp		WITH allt(d17comp)+iif(right(allt(d17comp),1)='/','','/')+'TE'	FOR 'CR'$d17comp and len(allt(d17comp))<=32
			REPLACE d17comp		WITH allt(d17comp)+iif(right(allt(d17comp),1)='/','','/')+'TD'	FOR 'DB'$d17comp and len(allt(d17comp))<=32
   endcase
endif
&& R.10e Mariano

* ACTUALIZACION DE DATOS DE LA VERSION 4.16
* -----------------------------------------
&& R.11b Mariano
if wVeDa <= '416' 	&& R.15 Mariano ( or (wveda='416' and w_actver)	)	&& R.19 Mariano (Puse <= en lugar de <)
	do case
		&& R.13b Mariano
		case wArchivo='IVF005'
			=vMensa('Version 4.16: Numero hasta de Comprobante en 05',200)
	         =masrow(1)
*	         set step on 		&& R.14 Mariano
*	         acti wind debug 	&& R.14 Mariano
    		locate for d5tipo='V' and at('/',d5nume)=9 and at('****',d5nume)>9
    		if found()
				if !stag('d5tipo', 'ivf005')
					use
					=db_indexiw(.t.,'f05')		&& indexo antes de abrir por las dudas que no haya indice
	    	        use(wFileName) exclu
				endif
				set order to tag d5tipo
	    		locate for d5tipo='V' and at('/',d5nume)=9 and at('****',d5nume)>9	&& R.14 Mariano
				do while .t.
*		    		locate for d5tipo='V' and at('/',d5nume)=9 and at('****',d5nume)>9	&& R.14 Mariano
		    		if found()
						scatter memvar
						w_rec05=recno()
						do while m.d5tipo=d5tipo and m.d5tico=d5tico and m.d5tifo=d5tifo and left(m.d5nume,9)=left(d5nume,9) and !eof()
							skip
						enddo	
						if m.d5tipo=d5tipo and m.d5tico=d5tico and m.d5tifo=d5tifo and left(m.d5nume,8)<left(d5nume,8)
							m.d5nume=left(m.d5nume,9)+strcero(str(val(left(d5nume,8))-1,8))
							go w_rec05
							replace d5nume with m.d5nume
						else
							go w_rec05
							replace d5nume with left(m.d5nume,9)
						endif
						&& R.14b Mariano
						if d5tipo='V' and at('/',d5nume)=9 and at('****',d5nume)>9
							exit
						endif
						&& R.14e Mariano
					else
						exit
					endif
					continue	&& R.14 Mariano
				enddo						
			endif
    		repla d5nume with left(d5nume,9)+iif(left(d5nume,8)<left(d5nume,4)+substr(d5nume,10,4), left(d5nume,4)+substr(d5nume,10,4), strcero(str(val(left(d5nume,4))+1,4))+substr(d5nume,10,4)) for len(allt(d5nume))=13 and d5tipo='V' and at('/',d5nume)=9
		&& R.13e Mariano

		&& R.14b Mariano
		case wArchivo='IVF006'
			=vMensa('Version 4.16: Numero hasta de Comprobante en 06',200)
			=masrow(1)
    		locate for d6tipo='V' and at('/',d6nume)=9 and at('****',d6nume)>9
    		if found()
				use &d0f05 in 0 alias f05 exclusive
				if !stag('d5inte', 'f05')
					use in f05
					=db_indexiw(.t.,'f05')
					use &d0f05 in 0 alias f05 orde d5inte
				endif
				set order to tag d5inte in f05
				select ivf006
	    		locate for d6tipo='V' and at('/',d6nume)=9 and at('****',d6nume)>9
				do while .t.
		    		if found()
						scatter memvar
						if seek(m.d6inte,'f05')
							m.d6nume=f05.d5nume
							replace d6nume with m.d6nume
							if d6tipo='V' and at('/',d6nume)=9 and at('****',d6nume)>9
								exit
							endif
						endif
					else
						exit
					endif
					continue
				enddo						
				use in f05
			endif
    		repla d6nume with left(d6nume,9)+iif(left(d6nume,8)<left(d6nume,4)+substr(d6nume,10,4), left(d6nume,4)+substr(d6nume,10,4), strcero(str(val(left(d6nume,4))+1,4))+substr(d6nume,10,4)) for len(allt(d6nume))=13 and d6tipo='V' and at('/',d6nume)=9
		
		case wArchivo='IVF007'
			=vMensa('Version 4.16: Numero hasta de Comprobante en 07',200)
			=masrow(1)
    		locate for d7tipo='V' and at('/',d7nume)=9 and at('****',d7nume)>9
    		if found()
				use &d0f05 in 0 alias f05 exclusive
				if !stag('d5inte', 'f05')
					use in f05
					=db_indexiw(.t.,'f05')
					use &d0f05 in 0 alias f05 orde d5inte
				endif
				set order to tag d5inte in f05
				select ivf007
	    		locate for d7tipo='V' and at('/',d7nume)=9 and at('****',d7nume)>9
				do while .t.
		    		if found()
						scatter memvar
						if seek(m.d7inte,'f05')
							m.d7nume=f05.d5nume
							replace d7nume with m.d7nume
							if d7tipo='V' and at('/',d7nume)=9 and at('****',d7nume)>9
								exit
							endif
						endif
					else
						exit
					endif
					continue
				enddo						
				use in f05
			endif
    		repla d7nume with left(d7nume,9)+iif(left(d7nume,8)<left(d7nume,4)+substr(d7nume,10,4), left(d7nume,4)+substr(d7nume,10,4), strcero(str(val(left(d7nume,4))+1,4))+substr(d7nume,10,4)) for len(allt(d7nume))=13 and d7tipo='V' and at('/',d7nume)=9
		&& R.14e Mariano
		
		&& R.12b Mariano
		CASE wArchivo='IVF012' and wReRoot
			=vMensa('Version 4.16: Actualiza campos en 12',200)
			=masrow(1)
			REPLACE p0diin		WITH "\IW\INTERFAZ" for empty(p0diin)	
		&& R.12e Mariano
		
		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.16: Actualiza comprobantes en 17',200)
			=masrow(1)
			REPLACE d17comp		WITH strtran(d17comp,'/TE','/TC')	FOR 'TE'$d17comp
   endcase
endif
&& R.11e Mariano

* ACTUALIZACION DE DATOS DE LA VERSION 4.17
* -----------------------------------------
&& R.15b Mariano
*if wVeDa < '417' or (wveda='417' and w_actver)	&& R.19 Mariano
if wVeDa <= '417' 	&& R.19 Mariano
	do case
		case wArchivo='IVF016' and wReRoot
			=vMensa('Version 4.17: Detalla Definiciones en 16',200)
			=masrow(1)
			replace d16deta with 'N' for empty(d16deta)			
			    
		&& R.16b Mariano
			if !stag('d16defi', warchivo)
				use
				=db_indexiw(.t.,'f16')		&& indexo por si no tengo indice
	            use &warchivo. in 0 alias (warchivo)
			endif
			set order to tag d16defi in (warchivo)
			&& Asigno numero a la definicion 200
			sele (warchivo)
			private w_pasa
			if !seek('200',warchivo) or d16nomb!='F.2002 - IVA POR ACTIVIDAD'
				w_pasa=.t.
			else
				w_pasa=.f.
			endif
			if !seek('200',warchivo) or w_pasa
				private w_aux
				w_aux=200
				locate for d16defi=str(w_aux,3)
				do while d16defi=str(w_aux,3) and !eof()
					w_aux=w_aux+1
					locate for d16defi=str(w_aux,3)
				enddo
				use \iw\fi\ivf016 in 0 alias f16
				sele f16
				repla d16defi with str(w_aux,3) for d16defi='200'		&& Modifico con el valor obtenido
				use				
				sele(warchivo)
				append from \iw\fi\ivf016.dbf for d16defi=str(w_aux,3)		&& tomo toda la definicion 200
			else 
				w_aux=200
			endif
			go top
		case wArchivo='IVF017' and wReRoot
			=vMensa('Version 4.17: Detalla Definiciones en 17',200)
			=masrow(1)
			replace d17deta with 'N' for empty(d17deta)			
			replace d17clas with left(d17clas,19)+'/ / ' for !empty(left(d17clas,19)) and empty(substr(d17clas,20,23))	&& R.17 Mariano

			if !stag('d17defi', warchivo)
				use
				=db_indexiw(.t.,'f17')		&& indexo por si no tengo indice
	            use &warchivo. in 0 alias (warchivo)
			endif
			set order to tag d17defi in (warchivo)
			&& Asigno numero a la definicion 200
			sele (warchivo)
			private w_pasa
			if !seek('200'+'008',warchivo) or d17titu!='VENTA DE COSAS MUEBLES, OBRAS, LOCACIONES Y/O PREST. SERVICIOS'
				w_pasa=.t.
			else
				w_pasa=.f.
			endif
			if !seek('200'+'008',warchivo) or w_pasa
				private w_aux
				w_aux=200
				locate for d17defi=str(w_aux,3)
				do while d17defi=str(w_aux,3) and !eof()
					w_aux=w_aux+1
					locate for d17defi=str(w_aux,3)
				enddo
				use \iw\fi\ivf017 in 0 alias f17
				sele f17
				repla d17defi with str(w_aux,3) for d17defi='200'		&& Modifico con el valor obtenido
				use				
				sele(warchivo)
				append from \iw\fi\ivf017.dbf for d17defi=str(w_aux,3)		&& tomo toda la definicion 200
			else 
				w_aux=200
			endif
			go top
		&& R.16e Mariano
	endcase
endif
&& R.15e Mariano

* ACTUALIZACION DE DATOS DE LA VERSION 4.18
* -----------------------------------------
&& R.19b Mariano
if wVeDa < '418' or (wveda='418' and w_actver)	
	do case
		case wArchivo='IVF000'
			=vMensa('Version 4.18: Actualiza Sin Asignar en Otros Conceptos en 00',200)
			=masrow(1)
			go top
			if p0prv1=4
				if 'Imp.Int'$p0ocv1
*					repla p0prv1 WITH 6	&& R.21 Mariano
					repla p0prv1 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0ocv1
					repla p0prv1 WITH 6
				endif
				&& R.21e Mariano
			endif
			if p0prv2=4
				if 'Imp.Int'$p0ocv2
*					repla p0prv2 WITH 6	&& R.21 Mariano
					repla p0prv2 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0ocv2
					repla p0prv2 WITH 6
				endif
				&& R.21e Mariano
			endif
			if p0prv3=4
				if 'Imp.Int'$p0ocv3
*					repla p0prv3 WITH 6	&& R.21 Mariano
					repla p0prv3 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0ocv3
					repla p0prv3 WITH 6
				endif
				&& R.21e Mariano

			endif
			if p0prc1=4
				if 'Imp.Int'$p0occ1
*					repla p0prc1 WITH 6	&& R.21 Mariano
					repla p0prc1 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0occ1
					repla p0prc1 WITH 6
				endif
				&& R.21e Mariano
			endif
			if p0prc2=4
				if 'Imp.Int'$p0occ2
*					repla p0prc2 WITH 6	&& R.21 Mariano
					repla p0prc2 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0occ2
					repla p0prc2 WITH 6
				endif
				&& R.21e Mariano
			endif
			if p0prc3=4
				if 'Imp.Int'$p0occ3
					repla p0prc3 WITH 6	&& R.21 Mariano
					repla p0prc3 WITH 5	&& R.21 Mariano
				endif
				&& R.21b Mariano
				if 'O. Conc.'$p0occ3
					repla p0prc3 WITH 6
				endif
				&& R.21e Mariano
			endif
			if p0lidc=0
				repla p0lidc with 5000
			endif
			
		case wArchivo='IVF005'
			=vMensa('Version 4.18: Sucursal de Comprobante en 05',200)
	         =masrow(1)
			replace d5sucu with '0'+d5sucu for len(allt(d5sucu))=4
			replace d5nume with '0'+d5nume for d5tipo='C' and empty(d5sucu) and at('-',d5nume)=5 and empty(right(d5nume,1))	&& R.20 Mariano

		case wArchivo='IVF006'
			=vMensa('Version 4.18: Sucursal de Comprobante en 06',200)

			=masrow(1)
			replace d6sucu with '0'+d6sucu for len(allt(d6sucu))=4

		case wArchivo='IVF007'
			=vMensa('Version 4.18: Sucursal de Comprobante en 07',200)
			=masrow(1)
			replace d7sucu with '0'+d7sucu for len(allt(d7sucu))=4

		case wArchivo='IVF014'
			=vMensa('Version 4.18: Sucursal de Comprobante en 14',200)
			=masrow(1)
			replace d14sucu with '0'+d14sucu for len(allt(d14sucu))=4
	endcase
endif
&& R.19e Mariano
use

* ACTUALIZACION DE DATOS DE LA VERSION que viene de DOS
* -----------------------------------------------------

if wVeDa < '400' and .f.

	=db_indexiw(.t.,'F'+right(warchivo,2))

endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.01
* -------------------------------------------
if wVeDa < '401' and .f.
	do case
		case wArchivo='IVF005'	&& agregue el tag d5comp similar al d5peri, pero d5tico antes de d5tifo
			=db_indexiw(.t.,'f05')
*		case wArchivo='IVF023'
*			=db_indexiw(.t.,'f23')
	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.02
* -------------------------------------------
if wVeDa < '402' and .f.
	do case
		case wArchivo='IVF023'
			=db_indexiw(.t.,'f23')
	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.05
* -------------------------------------------
if wVeDa < '405' and .f.
	do case
		case wArchivo='IVF017'
			=db_indexiw(.t.,'f17')
	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.06
* -------------------------------------------
if wVeDa <'406' and .f.
	do case
		case wArchivo='IVF001'
			=db_indexiw(.t.,'f01')

		case wArchivo='IVF005'
			=db_indexiw(.t.,'f05')

		case wArchivo='IVF016'
			=db_indexiw(.t.,'f16')

		case wArchivo='IVF017'
			=db_indexiw(.t.,'f17')

		case wArchivo='IVF023'
			=db_indexiw(.t.,'f23')

	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.07
* -------------------------------------------
if wVeDa <'407' and .f.
	do case
		case wArchivo='IVF005'
			=db_indexiw(.t.,'f05')
	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.08
* -------------------------------------------
if wVeDa <'408' and .f.
	do case
		case wArchivo='IVF001'
			=db_indexiw(.t.,'f01')

		case wArchivo='IVF005'
			=db_indexiw(.t.,'f05')
	endcase
endif

* ACTUALIZACION DE INDICES DE LA VERSION 4.09
* -------------------------------------------
if wVeDa <'409' and .f.
	do case
		case wArchivo='IVF005'
			=db_indexiw(.t.,'f05')
	endcase
endif

*===> ACTUALIZADOR NUEVO
* ACTUALIZACION DE INDICES A LA ULTIMA VERSION
* --------------------------------------------
*if wVeDa <='415'	&& R.14 Mariano
*if wVeDa <='416'	&& R.14 Mariano	&& R.15 Mariano
*if wVeDa <='417'	&& R.15 Mariano	&& R.19 Mariano
if wVeDa <='418'	&& R.19 Mariano

	** borro el indice del f01 para que cuando ingrese indexe todas las dbf de la empresa
	** ya que actualice todos por code_page a 1252. Ademas de esta manera indexa solo una vez.
	d0f01=strtran(upper(d0f01),'.DBF','')
	erase &d0f01..cdx		&& elimino el indice del f01, ya que si viene de DOS puede estar indexada
	do case
		case wArchivo='FEX' and wReRoot
			=db_indexiw(.t.,'FEX')
		case wArchivo='FFO' and wReRoot
			=db_indexiw(.t.,'FFO')
		case wArchivo='IVF010' and wReRoot
			=db_indexiw(.t.,'f10')
		case wArchivo='IVF012' and wReRoot
			=db_indexiw(.t.,'f12')
		case wArchivo='IVF016' and wReRoot
			=db_indexiw(.t.,'f16')
		case wArchivo='IVF017' and wReRoot
			=db_indexiw(.t.,'f17')
		case wArchivo='IVF018' and wReRoot
			=db_indexiw(.t.,'f18')
		case wArchivo='IVF021' and wReRoot
			=db_indexiw(.t.,'f21')
		case wArchivo='IVF022' and wReRoot
			=db_indexiw(.t.,'f22')
		case wArchivo='IVF023' and wReRoot
			=db_indexiw(.t.,'f23')
		case wArchivo='IVF024' and wReRoot
			=db_indexiw(.t.,'f24')
		case wArchivo='IVF025' and wReRoot
			=db_indexiw(.t.,'f25')
		case wArchivo='IVF066' and wReRoot
			=db_indexiw(.t.,'f66')
	endcase
endif

return ''

*-----------------------------------------------------------------------------
function num
*-----------------------------------------------------------------------------
@ wFila,64 say str(reccount(),7)+str(recno(),7)
return .t.

*-----------------------------------------------------------------------------
proc VMensa
*-----------------------------------------------------------------------------
para wMens,wTone
if parameters()=1
	wTone=1200
endif
*tone(wTone,1)
=masrow(1)
@ row(),01 say wMens
return

*-----------------------------------------------------------------------------
proc masrow
*-----------------------------------------------------------------------------
para SumLinea

wRow = Row()+1
if wRow>=22
	wRow = 22-SumLinea
	scroll 4,0,22,79,SumLinea
endif
@ wRow,00 say ''
return

*======================================================================
PROCEDURE cons999

SELE ssf090
SET FILTER TO errdate = DATE()	AND errtime => w_time AND UPPER(ALLT(errdata)) = 'ACTUALIZACION'

p_report	= 'verif'

w_print		= .F.

DO listalib

DO represet

RETURN
