*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP900P.PRG -> Parametros de la empresa
*)-H
*)-H Release		 : 03
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

w_curobj=_curobj	  &&(Salva numero de objeto en el read activo)
w_alan=alias()		  &&(Salva alias vigente al llamado del programa)
w_wind=wontop()	  &&(Salva ventana vigente al llamado del programa)
w_rdlevel=rdlevel() &&(Salva Nro. de read activo)

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

*) Datos del archivo a mantener------------------------------------------------

w_pref=''
w_pfile='900p'
w_alias='f00'
w_title=db_name(w_alias)

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F04'
fieldvec[4]='F05'
fieldvec[5]='F13'

use (d0f00) in 0 alias 'f00' EXCLUSIVE			&&(Parametros)
use (d0f01) in 0 alias 'f01' order d1tip		&&(Tablas)
use (d0f04) in 0 alias 'f04' order d4peri		&&(Periodos)
use (d0f05) in 0 alias 'f05' order d5peri		&&(Comprobantes)
use (d0f13) in 0 alias 'f13' order 'd13peri'	&&(Anticipos y retenciones)

=creatipos('V')									&& clase de operacion

*SELE f00
*SET RELA TO "JU" + p0juce	INTO f01	ADDITIVE

*SELE f01
*SET RELA TO "JD" + d1tdgi	INTO f01t	ADDITIVE 

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	=borratipos()
	return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
go top
scatter memvar

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','all',.t.)
=skipbar('registro','Agregar'   ,.f.)
=skipbar('registro','Modificar',.f.)
=skipbar('registro','Elegir',.f.)
=skipbar('registro','Calculadora',.f.)
=skipbar('registro','Salir',.f.)

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

&& R.02b Mariano
***vfp***
if version()='Visual FoxPro'
    on key label esc do actsalir
endif
***vfp***
&& R.02e Mariano

do iws900p.spr

&& R.02b Mariano
***vfp***
if version()='Visual FoxPro'
    on key label esc
endif
***vfp***
&& R.02e Mariano

pop key
pop menu _msysmenu
=closall()
=borratipos()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu900p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu900p0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu900p0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid900p(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

if begintran()

	wait wind 'Actualizando parแmetros...' time .4

	sele (w_alias)

	REPLA p0emp		WITH d0emp
	m.p0emp=d0emp	&& R.01 Mariano
	
	d0dir =trim(m.p0dir)
	d0tlp =trim(m.p0tlp)
	d0imp =m.p0coim
	d0imfa=m.p0imfa
	d0coim=m.p0coim
	d0macc=m.p0macc
	d0emcg=m.p0emcg
	d0comu=m.p0comu
	d0juce=m.p0juce
	d0poce=m.p0poce
*===>
	d0cciva	= m.p0cciva
	d0afip	= m.p0afip
	d0gan	= m.p0gan
			
d0ibvta		= .F.
d0ibcpra	= .F.
d0ganvta	= .F.
d0gancpra	= .F.

IF m.p0prv1 <> 1	AND m.p0prv2 <> 1	AND m.p0prv3 <> 1
		d0ibvta		= .T.
ENDIF

IF m.p0prc1 <> 1	AND m.p0prc2 <> 1	AND m.p0prc3 <> 1
		d0ibcpra	= .T.
ENDIF

IF m.p0prv1 <> 2	AND m.p0prv2 <> 2	AND m.p0prv3 <> 2
		d0ganvta	= .T.
ENDIF

IF m.p0prc1 <> 2	AND m.p0prc2 <> 2	AND m.p0prc3 <> 2
		d0gancpra	= .T.
ENDIF

	gather memvar

	=inico() &&(inicializacion de variables contables)

	=endtran()

	clear read

endif
return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc when900p
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

*No va el irclave porque no hay clave

d0afip = " "

if !empty(m.p0juce)
	w_juce=iif(seek('JU'+m.p0juce,'f01'),f01.d1des,'')
	show get w_juce

	d0afip		= IIF(SEEK('JD' + f01.d1tdgi,'f01t'),f01t.clav,'')
	
endif

m.p0afip	= d0afip

if !empty(m.p0emcg)

*===>
		w_d0paco	= d0paco
		IF	(m.p0cciva = "S"	 .AND.	d0paco	= "\CG\")
					w_d0paco	= "CO\"
		ENDIF
		IF	(m.p0cciva = "N"	 .AND.	d0paco	= "CO\")
					w_d0paco	= "\CG\"
		ENDIF
		
*		if mfile(d0paco+'cof010.dbf')
*			ncf10=d0paco+'cof010'

		IF mfile(	w_d0paco+'cof010.dbf')
			ncf10	=	w_d0paco+'cof010'
*===>
			fieldvec[1]='F10C'
			do while .t.
				use (ncf10) in 0 alias 'f10c' order p0coem
				if !neterr()
					exit
				endif
			enddo
			fieldvec=''

			w_emcg=iif(seek(m.p0emcg,'f10c'),f10c.p0empr,'')
			sele f10c
			use
		else
			w_emcg=''
		endif
		show get w_emcg
endif

if w_vari='P0COIM'
	go top in f04
	if !eof('f04')
		w_retu=.f.
	endif
endif

=seek('JU'+m.p0juce,'f01')
if m.p0comu='N' or !empty(f01.d1coef)
	show get p0poce disable
else
	show get p0poce enable
endif

for i=1 to 3
	* Compras
	w_vprv='w_prv'+(str(i,1))
	w_cprv='m.p0prv'+(str(i,1))
	do case
		case &w_cprv=1
			&w_vprv='IB P/R'
		case &w_cprv=2
			&w_vprv='Gan. P/R'
		case &w_cprv=3
*			&w_vprv='Otros'	&& R.03 Mariano
			&w_vprv='Otros Tributos'	&& R.03 Mariano
		&& R.03b Mariano
		case &w_cprv=4
			&w_vprv='Impto. Municipal'
		case &w_cprv=5
			&w_vprv='Impto. Interno'
		&& R.03e Mariano
		otherwise
			&w_vprv='Sin Asignar'
	endcase
	* Ventas
	w_vprc='w_prc'+(str(i,1))
	w_cprc='m.p0prc'+(str(i,1))
	do case
		case &w_cprc=1
			&w_vprc='IB P/R'
		case &w_cprc=2
			&w_vprc='Gan. P/R'
		case &w_cprc=3
*			&w_vprc='Otros'	&& R.03 Mariano
			&w_vprc='Otros Tributos'	&& R.03 Mariano
		&& R.03b Mariano
		case &w_cprc=4
			&w_vprc='Impto. Municipal'
		case &w_cprc=5
			&w_vprc='Impto. Interno'
		&& R.03e Mariano
		otherwise
			&w_vprc='Sin Asignar'
	endcase
	show get &w_vprv
	show get &w_vprc
endfor

SHOW GETS

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid900p
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

&& R.02b Mariano
***vfp***
if version()='Visual Fox' and (w_pfile!='900p' or (wontop()!='IWS900P' and wontop()!='IWS900P'))
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***
&& R.02e Mariano

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='P0COIM' or w_show
	if !m.p0coim$'123'
		if !w_show
			wait wind 'Opciones vแlidas: 1,2,3...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0IMFA' or w_show
	if !m.p0imfa$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0COMU' or w_show
	if !m.p0comu$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
endif

*===>
IF w_vari = 'P0GAN' or w_show
	if !m.p0gan$'FJ' 
		if !w_show
			wait wind 'Opciones vแlidas: F,J...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='P0CCIVA' or w_show
	IF m.p0cciva = "S"
					m.p0emcg = "99"
					SHOW GET m.p0emcg DISABLE
	ELSE
		SHOW GET m.p0emcg ENABLE
	ENDIF
	if !m.p0cciva$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
	IF w_vari	= 'P0CCIVA'
				_curobj	= objnum(m.p0emcg)
				SHOW GETS
	ENDIF
endif
*===>

if w_vari='P0EMCG' or w_show

	if lastkey()=p_f4
		do brow900p
	endif
	if !empty(m.p0emcg)
		m.p0emcg=strcero(m.p0emcg)
		show get m.p0emcg
*===>
		w_d0paco	= d0paco
		IF	(m.p0cciva = "S"	 .AND.	d0paco	= "\CG\")
					w_d0paco	= "CO\"
		ENDIF
		IF	(m.p0cciva = "N"	 .AND.	d0paco	= "CO\")
					w_d0paco	= "\CG\"
		ENDIF
		
*		if mfile(d0paco+'cof010.dbf')
*			ncf10=d0paco+'cof010'

		IF mfile(	w_d0paco+'cof010.dbf')
			ncf10	=	w_d0paco+'cof010'
*===>
				fieldvec[1]='F10C'
				do while .t.
					use (ncf10) in 0 alias 'f10c' order p0coem
					if !neterr()
						exit
					endif
				enddo
				fieldvec=''

				if !seek(m.p0emcg,'f10c')
					wait wind 'Empresa Contable Inexistente...'
					w_retu=.f.
				else
					w_emcg=f10c.p0empr
					show get w_emcg
				endif
				sele f10c
				use

			else

				=advgrave('Sistema No Instalado')
				w_retu=.f.

			endif
	else
		w_emcg=''
		show get w_emcg
	endif

endif

if w_vari='P0LEOT' or w_show
	if empty(m.p0leot)
		m.p0leot='Otros'
		show get m.p0leot
	endif
endif

&& R.03b Mariano
if w_vari='P0LIDC' .or. w_show
	if m.p0lidc<0
		if !w_show
			wait wind 'El Limite debe ser positivo' nowait
		endif
		w_retu=.f.
	endif
endif
&& R.03e Mariano

if w_vari='P0JUCE' or w_show
	do fhelp with 'P0JUCE','JU','iwp001','f01','d1cla','d1des','Juridicci๓n inexistente',(p_char),'w_retu'
endif

=seek('JU'+m.p0juce,'f01')

d0afip		= IIF(SEEK('JD' + f01.d1tdgi,'f01t'),f01t.clav,'')
m.p0afip	= d0afip

if m.p0comu='N' or !empty(f01.d1coef)
	m.p0poce=0
	show get m.p0poce disable

	=ultdepri('P0DIR','m.p0comu')
	=prideult('P0COMU','m.p0comu')
else

	show get m.p0poce	enable

	=ultdepri('P0DIR','m.p0poce')
	=prideult('P0POCE','m.p0poce')
endif
for i=1 to 3	
	* Compras
	w_vprv='w_prv'+(str(i,1))
	w_cprv='m.p0prv'+(str(i,1))
	do case
		case &w_vprv='IB P/R'
			&w_cprv=1
		case &w_vprv='Gan. P/R'
			&w_cprv=2
*		case &w_vprv='Otros'	&& R.03 Mariano
		case &w_vprv='Otros Tributos'	&& R.03 Mariano
			&w_cprv=3
		&& R.03b Mariano
		case &w_vprv='Impto. Municipal'
			&w_cprv=4
		case &w_vprv='Impto. Interno'
			&w_cprv=5
		&& R.03e Mariano
		case &w_vprv='Sin Asignar'
			&w_cprv=6	&& R.03 Mariano (6 x 4)
	endcase
	* Ventas
	w_vprc='w_prc'+(str(i,1))
	w_cprc='m.p0prc'+(str(i,1))
	do case
		case &w_vprc='IB P/R'
			&w_cprc=1
		case &w_vprc='Gan. P/R'
			&w_cprc=2
*		case &w_vprc='Otros'	&& R.03 Mariano
		case &w_vprc='Otros Tributos'	&& R.03 Mariano
			&w_cprc=3
		&& R.03b Mariano
		case &w_vprc='Impto. Municipal'
			&w_cprc=4
		case &w_vprc='Impto. Interno'
			&w_cprc=5
		&& R.03e Mariano
		case &w_vprc='Sin Asignar'
			&w_cprc=6	&& R.03 Mariano (6 x 4)			
	endcase
endfor

sele (w_alias)
=modicampo()

return(w_retu)

*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	Proce Brow900p
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ


**===> Vengo acแ xq NO USA Contab. IVA y si tengo CO\ en d0paco, muestra SOLO LA 99
w_d0paco	= d0paco
IF	(m.p0cciva = "S"	 .AND.	d0paco	= "\CG\")
				w_d0paco	= "CO\"
ENDIF
IF	(m.p0cciva = "N"	 .AND.	d0paco	= "CO\")
				w_d0paco	= "\CG\"
ENDIF
		
*if mfile(d0paco+'cof010.dbf')
*	ncf10=d0paco+'cof010'

IF mfile(	w_d0paco+'cof010.dbf')
	ncf10	=	w_d0paco+'cof010'
*===>

	fieldvec[1]='F10C'
	do while .t.
		use (ncf10) in 0 alias 'f10c' order p0coem
		if !neterr()
			exit
		endif
	enddo
	fieldvec=''

 	sele f10c
 		
	set order to 1
	=seek(m.p0emcg,'f10c')
	if eof('f10c')
		go top in f10c
	endif

	&& R.02b Mariano
	***vfp***
	if version()='Visual FoxPro'
		w_title='EMPRESAS DE CONTABILIDAD'
		if !wexist('w_hcom')
			define wind w_hcom at 0,0 size 23,138;
				noclose nominimize float system font 'arial',9 title(w_title)
			move wind w_hcom center
		endif
		if wvisible("w_hcom")
			activate window w_hcom same
		else
			activate window w_hcom noshow
		endif

		private k_busca,k_proc
		k_busca=' '
		k_proc='wbrow900p()'
		@ 0.000,0.000 get k_busca ;
			size 0.001,0.001 ;
			default " " ;
			font "Fixedsys", 9 ;
			picture "@!" style 'T'	&&	color ,rgb(,,,255,255,255)

		if not wvisible("w_hcom")
			activate window w_hcom
		endif

		on key label esc

		read when &k_proc

	else
	***vfp***
		push menu _msysmenu
		set skip of menu _msysmenu .t.

*		defi wind w_most from 0,0 to 15,79 color scheme 10
		w_title='EMPRESAS DE CONTABILIDAD'
		if !wexist('w_hcom')
			define wind w_hcom at 0,0 size 23,138;
				noclose nominimize float system font 'arial',9 title(w_title)
			move wind w_hcom center
		endif
	&& R.02e Mariano


		browse  ;
			fields p0coem :h='C๓digo':v=fenter() :f ,;
				p0empr :h='Nombre' ,;
			p0path :h='Path'    ;
			nomenu ;
			noedit ;
			nodelete ;
			valid :f salebrow(lastkey()) ;
			font 'Arial', 10 ;
			noappend wind w_hcom	&& R.02 Mariano

*			title 'EMPRESAS DE CONTABILIDAD' 	&& R.02 Mariano
*			window w_most						&& R.02 Mariano

*			rele wind w_most	&& R.02 Mariano
			rele wind w_hcom	&& R.02 Mariano

			keyboard '{ENTER}'
			=inkey()
			w_modicampo=.t.
			m.p0emcg=F10c.p0coem
			w_emcg=f10c.p0empr
		
			show get m.p0emcg
			show get w_emcg
	
		pop menu _msysmenu	&& R.02 Mariano
	endif	&& R.02 Mariano

	sele f10c
	use
	sele (w_alias)

endif

return

&& R.02b Mariano
***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC wbrow900p &&(Help's de empresas de contabilidad
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	push menu _msysmenu
	set skip of menu _msysmenu .t.
	w_macr=on('KEY','ESC')
	on key label esc
	if !wexist('w_hcom')
		define wind w_hcom at 0,0 size 23,138;
			noclose nominimize float system font 'arial',9 title(w_title)
		move wind w_hcom center
	endif

	deact wind w_hcom

	browse  ;
		fields p0coem :h='C๓digo':v=fenter() :f ,;
			p0empr :h='Nombre' ,;
			p0path :h='Path'    ;
		nomenu ;
		noedit ;
		nodelete ;
		valid :f salebrow(lastkey()) ;
		font 'Arial', 10 ;
		noappend wind w_hcom

	keyboard '{ENTER}'
	=inkey()
	w_modicampo=.t.
	m.p0emcg=F10c.p0coem
	w_emcg=f10c.p0empr
	
	show get m.p0emcg
	show get w_emcg
	

	rele wind w_hcom

	pop menu _msysmenu
return
***vfp***
&& R.02e Mariano

