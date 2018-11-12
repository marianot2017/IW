	*-P -----------------------------------------------------------------------------
*  IWP950       Recupero
*  Release : 01
*-P -----------------------------------------------------------------------------
return
*-P -----------------------------------------------------------------------------
procedure empr950
*-P -----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Parametro NF_StRe --> se utiliza en eleccion de empresa cuando
*                       se ha detectado una salida anormal en un
*                       proceso transaccional invocando EMPR950.
para nf_stre
*
* Si no viene de Eleccion de Empresa se setea la variable.
*-----------------------------------------------------------------------------

w_para=parameters()
if w_para=0
	nf_stre=.f.
endif


if .not. hayspace(2000000)
	read when .f.
	return
endif

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso.')
	read when .f.
	return
endif

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu
set skip of menu _msysmenu .t.


if !nf_stre

	wait 'Este proceso realiza el RECUPERO de'+chr(13)+;
		'los ARCHIVOS copiados por medio de'+chr(13)+;
		'la opción de resguardo de empresa.'+chr(13)+;
		'CUIDADO, LO ACTUAL SE PERDERA !!!' wind nowait	&& R.01 Mariano (saque a diskette y reestructure)

endif

s_sino = 2
wcomver=.t.

do while .t.
	if sino(' Confirma Recupero de Archivos de Empresa ? ','NO',upper(prompt()))='S'	&& R.01 Mariano (if sino(' Confirma Recupero de Archivos de Empresa ? ')='S')
		*abre(d0f10,'x')
		* Si NF_StRe esta en .f. se abre file si no ya esta abierto.
		* -------------------------------------------------------------
		if !nf_stre

			fieldvec[1] = 'F10'
			use &d0f10 in 0 alias f10 order p0coem

			if neterr()
				exit
				*return
			endif

			=seek(d0coem,'f10')

			if .not. rlock('f10')
				=errornet(054)
				sele f10
				use
				exit
				*return
			endif
		endif

		* si esta en blanco no viene de eleccion de empresas.
		* ---------------------------------------------------
		if iif(!nf_stre,lockemp(nf_coem),nf_stre)

			&& R.01b Mariano
*			w_dire=''
*			wdrf=elijedisp('ORIGEN DEL RECUPERO')

*			wcomver=compvers(wdrf+w_dire)
			&& R.01e Mariano

			sele f10

			if .t.	&& R.01 Mariano (wcomver)
				priv wdire,wdriv
				wdire = alltrim(p0path)
				wdriv = wdr
				*use

				wpunt=at(':',wdire)
				if wpunt<>0
					wdriv = left(wdire,wpunt)
					wdire = subs(wdire,wpunt + 1,100)
				endif

				w66empr = wdriv +wdire+'I'+d0coem+netuser+'66.dbf'
				w66back = wdriv +'\IW\I'+d0coem+netuser+'66.dbf'
				*         copy file &w66Empr to &w66Back

				*% Antes de recuperar renombro los viejos
				*         wMacro='REN '+wDriv+wDire+'I*.* '+'-*.* > NUL'
				*         run   &wMacro

*				=renombrar(wdriv+wdire+'I*.* ', '-')	&& R.01 Mariano


				*%

*				wrecu=recupero( wdrf,w_dire, '*.*', wdriv, wdire, 'IW', d0coem, 'EM' )	&& R.01 Mariano

				&& R.01b Mariano
				wrecu=.f.
				do iwp900ur with 'E'
				&& R.01e Mariano				

				if .f.	&& R.01 Mariano
				if wrecu
					* Termino Bien
					* ------------

					*% Como termino bien  Borro los viejos
					*            wMacro='DEL '+wDriv+wDire+'-*.*  > NUL'
					*            run   &wMacro

					=delefile(wdriv+wdire, '-*.*')

					*%

					* ACTUALIZACION \IW\IVF010
					* ------------------------
					wescapar=.f.
*					wempresas=wdire+'Empresas.dbf'	&& R.01 Mariano
					wempresas=wdire+'Empr_ver.dbf'	&& R.01 Mariano
					if mfile(wempresas)
						sele 0
						use (wempresas)
						locate for p0coem=d0coem
						if !eof()
							wvein=p0vein
							wvees=p0vees
							wveda=p0veda
						endif
						use
						sele f10
						if seek(d0coem,'f10')
							if p0vees+p0veda <> wvees+wveda
								repl p0vein with wvein;
									,p0vees with wvees;
									,p0veda with wveda
								wescapar=.t.
							endif
						endif
						dele file (wempresas)

						* Aqui fuerzo la salida Aunque por version no haga falta
						* para que ingrese e inicialice obligadamente la empresa
						* A partir version 400 06
						wescapar=.t.
						*
					else
						wescapar=.t.
						sele f10
						if seek(d0coem,'f10')
							repl p0vein with '001';
								,p0vees with '001';
								,p0veda with '001'
						endif
					endif

					* Fuerza Grabacion        
					*         +                      +
					=audi('RCE S','Exitoso','',d0coem)

					=cleartran(nf_coem)

					if wescapar
						*               keyboard '{ALT+S}'
						ww_esca=.t.
						=mquit()
					endif

				else
					* Termino mal
					* ------------

					*%
					if adir(Files,wdriv+wdire+'-*.dbf')>0
						* Como termino mal borro los que pudo haber recuperado
						set cons off

						=delefile(wdriv+wdire, 'I*.*')

						set cons on

						=renombrar(wdriv+wdire+'-*.* ', 'I')

					else
						wait 'Hubo algún problema en el RECUPERO. La Información NO ES CONFIABLE' wind
					endif
					*%

					*nf_stre=.f.

					* Fuerza Grabacion             
					*         +                         +
					=audi('RCE S','Hubo Problemas','',d0coem)

				endif
				endif	&& R.01 Mariano

				dele file &w66back

			endif
		else
			=errornet(050)
		endif

		if used('f10') and !nf_stre
			sele f10
			use
		endif
		nf_stre=.f.

	else
		if used('f10') and !nf_stre
			sele f10
			use
		endif

		nf_stre=.f.
	endif
	exit
enddo

=unlockemp(nf_coem)

wait clear
pop key
pop menu _msysmenu
read when .f.

return nf_stre

*-P -----------------------------------------------------------------------------
procedure sist950
*-P -----------------------------------------------------------------------------

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso')
	read when .f.
	return
endif

=advsuave('Deberá salir del sistema entrar al UTIL y ejecutar la opción RECUPERO COMPLETO')
*do sist950.spr

return

*-----------------------------------------------------------------------------
procedure root950
* -----------------------------------------------------------------------------

if w0mop$'D'
	=advsuave('Versión Demostración. Inhibido el acceso')
	read when .f.
	return
endif

wait 'Este proceso realiza el RECUPERO de los ARCHIVOS copiados'+chr(13)+;
	 'por medio de la opción de Resguardo de Archivos del Sistema'+chr(13)+;
 	 'CUIDADO, LO ACTUAL SE PERDERA !!!' wind nowait	&& R.01 Mariano (saque a diskette y reestructure)

push key clear      && Salva estados de teclas
push menu _msysmenu && Salva estado de menu
set skip of menu _msysmenu .t.

wcover=.t.
if sino('Confirma Recupero de Archivos de Sistema ?','NO',upper(prompt()))=='S'	&& R.01 Mariano (if sino('Confirma Recupero de Archivos de Sistema ?')=='S')

	w_dire=''
*	wdrf=elijedisp('ORIGEN DEL RECUPERO')	&& R.01 Mariano

*	wcover=compver(wdrf+w_dire)		&& R.01 Mariano
	if .t.	&& wcover	&& R.01 Mariano
		d0mene=.f.

	  if .f.	&& R.01 Mariano
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
	  endif	&& R.01 Mariano

		&& R.01b Mariano				
*		=recupero(wdrf,w_dire,'*.*',wdr,'\IW\', 'IW', d0coem, 'RA')
		do iwp900ur with 'S'
		&& R.01e Mariano				

*		=unlockemp('')	&& R.01 Mariano
	endif
endif

sele f77
use

=login('S')
=login('E',.t.)

wait clear
pop key
pop menu _msysmenu
read when .f.

return
