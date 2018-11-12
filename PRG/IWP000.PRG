*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP000.PRG -> Programa principal
*)-H
*)-H Release         : 26
*)-H Fecha de inicio :
*)-H Fecha de fin.   :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS p_par1,p_par2,p_par3
PUBLIC w_show 
if parameters()=0
	p_par1='NO'
endif

if parameters()<=1
	p_par2='NO'
endif

if parameters()<=2
	p_par3='NO'
endif

set help off
set sysmenu to

*if file('iwp000.exe')						&& R.15 Mariano
if file('iwp000.exe') or file('iwpvfp.exe')	&& R.15 Mariano
	if !file('develop.')
		set development off
	else
		set development on
		delete file develop.
	endif
else
	set development on
endif

&& R.16b Mariano
*if set('DEVELOP')='ON'
*	set safe off
*	save macros to system
*endif

if set('DEVELOP')='ON'
	set safe off
	save macros to system
	close databases
	if file('iwp000.err')		&& R.18 Mariano (cgp000.err)
		modi file iwp000.err	&& R.18 Mariano (cgp000.err)
		do mquit in library
	endif
endif

if version()>='Visual FoxPro 09'
	a='_Screen.themes = .F.'
	&a
endif
&& R.16e Mariano

_REFOX_=(9876543210)
_REFOX_=(9876543210)

PUBLIC lMemu,csalida,d0f77, pp0coem

csalida=''
d0f77='ivf077'
pp0coem = ""

*===>
PUBLIC	pc_pr, pc_imp, pc_tota , pn_n			&&		para rutina de retenciones (ver al final)
PUBLIC	d0ibvta, d0ibcpra, d0ganvta, d0gancpra	&& para determinar si tiene IB y GAN definidas y habilitar o deshabilitar seg๚n corresponda

d0ibvta		= .F.
d0ibcpra	= .F.
d0ganvta	= .F.
d0gancpra	= .F.

pc_pr	= ""									&&		f00.pr {c/v} {1/2/3}
pc_imp	= ""									&&		d4 {c/v} im {6/7/8}
pc_tota	= ""									&&		d4ctot o d4vtot
pn_n	= 0										&&		= 4/6/7/8		4 = SIN ASIGNAR

w_iva	= 0
w_ib	= 0
w_gan	= 0
w_ot	= 0
w_while = " "

p_tdoc	= "80.86.87.99"

PUBLIC	d0pic
d0pic		= '99/9999'			&& R.03 Ra๚l picture del perํodo de la pantalla IWSCPRA2

PUBLIC array fieldvec(225)
fieldvec=''

on error do ssp090 with error(),lineno(),program()

deac menu _msysmenu

&& R.15b Mariano
if set('DEVELOP')='ON'
	d0cami='\SS_FOX\BMP;\SS_FOX;\SS_FOX\ERROR;\SS_FOX\IN;\SS_FOX\UT;PRG;MENU;SCREEN;DOC;FI;FI\000;IMPRESIO;LABEL;LISTAS;O_PAGO;REPORT;FORM;'
	set path to &d0cami
endif
&& R.15e Mariano

w_title=''
modi wind screen;
	title w_title ;
	fill file MemoFond.bmp;
	font 'arial',10 icon file iw.ico

zoom wind screen max


&& R.18b Mariano
*** cuando se ejecuta como Administrador y no esta la unidad disponible la crea automaticamente ***
if at(':',sys(5))=0 and at(':',p_par2)!=0
	w_netuse='run /N2 net use '+p_par2+' '+sys(5)
	&w_netuse
	w_defa='set defa to '+p_par2+'\IW'
	&w_defa
	p_par2='NO'
endif
&& R.18e Mariano

do iwp999

w_prog='PRINCIPAL'
do ssp001u

if wsuper
	do flock77	&& Entro como supervisor tomo exclusivo el f77
endif

do iwm000.mpr

w_title=space(44)+w_sys

set status bar on

csalida=w0mop+' '+w_ver+'   '+nf_text+'   Usuario : "'+ps_user+'"'

set message to csalida

*modi wind screen;
	title w_title ;
	fill file MemoFond.bmp;
	font 'arial',10

*zoom wind screen max

** VER CLAVES DE ACCESO AL SISTEMA COMO QUEDA EL NIVEL DEL USUARIO
public w_nivel
w_nivel='1'

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Eleccion de empresas
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
winflag=.t.

PUBLIC d0imfa,d0comu,d0emcg, d0afip, d0gan
store '' to d0imfa,d0comu,d0emcg, d0afip, d0gan

do iwp010e

&& R.01b Ra๚l
if mfile('indexar.mem') or mfile(d0coem+'\indexar.mem') or !mfile(d0f01+'.cdx')		
   do db_indexiw with .f.
   dele file indexar.mem
   dele file &d0coem.\indexar.mem
   dele file indexar.mem
endif
&& R.01e Ra๚l

set message to csalida

set cursor on

if wsuper	&& modo supervisor, se setea en ssp001u
	do setprot	&& elije perfil a proteger/desproteger
endif

p_read=.f. &&(controla salida del read valid fvread)
read valid fvread()

deac menu _msysmenu

=logout()

=Mquit()

return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC CompVers
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS C_Dr

if mfile(C_Dr+'\NUL')
	if mfile(C_Dr+'\EMPRESAS.DBF')
		*abre(C_Dr+'\EMPRESAS')
		w_file=C_Dr+'\EMPRESAS'
		use &w_file alias empresas in 0
		sele empresas

		loca for p0CoEm=f10->p0CoEm

		if !Eof() .and. (p0VeEs>wVersExe .or. p0VeDa>wVersExe)
			wcomver=.f.
			*mensa('No se puede Recuperar una Versiขn Mayor a la Actual')
			wait 'No se puede Recuperar una Versi๓n Mayor a la Actual' wind time 4
		endif
		use
	endif
else
	wait 'ERROR: Diskettera Abierta ๓ Diskette Da๑ado' wind time 4
	wcomver=.f.
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC CompVeR
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS C_Dr

if mfile(C_Dr+'\NUL')
	if mfile(C_Dr+'ivf010.DBF')

		w_file=C_Dr+'ivf010'
		use &w_file in 0 alias ivf010
		sele ivf010

		if defined('p0VeIn')
			do while !Eof()
				if p0VeEs>wVersExe .or. p0VeDa>wVersExe
					wcover=.f.
					wait 'No se puede Recuperar una Versi๓n Mayor a la Actual...' wind time 4
					exit
				endif
				skip
			enddo
		endif
		use
	endif
else
	wait 'ERROR: Diskettera Abierta ๓ Diskette Da๑ado...' wind time 4
	wcover=.f.
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC db_indexiw
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters i_pack, i_arch
private all like i_*

fieldvec[1] = 'F98'

if !used('f98')
	f_acce=1
	do while .t.
		fieldvec[1] = 'F98'
		use &d0f98 in 0 alias f98 order d98alia
		if !neterr()
			exit
		else
			if f_acce>=3	&& tres intentos
				return
			endif
			f_acce=f_acce+1
		endif
	enddo
endif

fieldvec=''

if !order('f98')='D98ALIA'
	set order to d98alia in f98
endif

sele f98

if type('i_arch')='C'
	set filter to upper(alltrim(d98alia))=upper(alltrim(i_arch))
endif

p_updtherm=.t.
p_acctherm=0
p_tottherm=reccount()

do acttherm with 'Verificando archivos...'

go top in f98

i_reccount=0

do while .not. eof('f98')

	i_file=trim(f98->d98file)
	i_alia=trim(f98->d98alia)

	=fupdtherm()

	if empty(f98->d98in01) or (empty(i_arch) and f98.d98sist)
		skip in f98
		loop
	endif

	f_acce=1
	do while .t.

		fieldvec[1] = i_alia

		if used(i_alia)
			use
		endif

		i_file='d0'+i_alia

		i_file=&i_file

		use (i_file) in 0 alias (i_alia) exclusive

		if !neterr()
			exit
		else
			if f_acce>=3	&& tres intentos
				do deactthermo
				sele f98
				set filter to
				return
			endif
			f_acce=f_acce+1
		endif

	enddo

	sele (i_alia)
	i_reccount=i_reccount+reccount()
	use
	skip in f98

enddo

=fupdtherm()

p_acctherm=0
p_tottherm=i_reccount

if i_pack
	p_text1='Compactando archivos...'
else
	p_text1='Generando indices...'
endif

do acttherm with p_text1

go top in f98

do while .not. eof('f98')

	i_file=trim(f98->d98file)
	i_alia=trim(f98->d98alia)

	if empty(f98->d98in01) or (empty(i_arch) and f98.d98sist)
		skip in f98
		loop
	endif

	=fupdtherm(f98->d98desc)

	f_acce=1
	do while .t.

		fieldvec[1] = i_alia

		i_file='d0'+i_alia

		i_file=&i_file

		use (i_file) in 0 alias (i_alia) exclusive

		if !neterr()
			exit
		else
			if f_acce>=3	&& tres intentos
				do deactthermo
				sele f98
				set filter to
				return
			endif
			f_acce=f_acce+1
		endif

	enddo

	sele (i_alia)

	dele tag all

	p_acctherm=p_acctherm+reccount()
	=fupdtherm()

	if i_pack

		pack

	endif

	for i_x=1 to 20
		i_s=strcero(str(i_x,2))

		if type('f98.d98in&i_s')='U'
			exit
		endif

		i_i=trim(f98->d98in&i_s)
		i_t=trim(f98->d98ta&i_s)
		if .not. empty(i_i)
			set orde to 0 in (i_alia)
			index on &i_i tag (i_t)
		endif
	next

	use

	skip in f98

enddo

sele f98
set filter to

do deactthermo

wait clear

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC db_indexco &&(Indexa Archivos de Contabilidad.)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if d0emcg='99' .and. imco() .and. mfile(d0paco+'IVA')

	wait wind nowait 'Clasificando Archivos Contables...'

	if mfile(d0paco+'cof010.dbf')
		ncf10=d0paco+'cof010'
		fieldvec[1]='F10C'
		do while .t.
			use (ncf10) in 0 alias 'f10c' EXCLUSIVE
			if !neterr()
				exit
			endif
		enddo
		fieldvec=''
		sele f10c
		pack
		inde on p0coem tag p0coem
		use
	endif

	if mfile(w0fipre+'01.dbf')
		ncf01=w0fipre+'01'
		fieldvec[1]='F01C'
		do while .t.
			use (ncf01) in 0 alias 'f01c' EXCLUSIVE
			if !neterr()
				exit
			endif
		enddo
		fieldvec=''
		sele f01c
		pack
		index on d1cuen tag d1cuen
		index on upper(d1desc) tag d1desc
		use
	endif

	if mfile(w0fipre+'02.dbf')
		ncf02=w0fipre+'02'
		fieldvec[1]='F02C'
		do while .t.
			use (ncf02) in 0 alias 'f02c' EXCLUSIVE
			if !neterr()
				exit
			endif
		enddo
		fieldvec=''
		sele f02c
		pack
		index on d2tipo+d2asie+d2reng tag d2tipo
		index on d2tipo+dtos(d2fech)+d2asie+d2reng tag d2tife
		index on d2tipo+d2cuen+dtos(d2fech)+d2asie+d2reng tag ticu
		use
	endif

	if mfile(w0fipre+'03.dbf')
		ncf03=w0fipre+'03'
		fieldvec[1]='F03C'
		do while .t.
			use (ncf03) in 0 alias 'f03c' EXCLUSIVE
			if !neterr()
				exit
			endif
		enddo
		fieldvec=''
		sele f03c
		pack
		index on d3tipo+d3asie tag d3tipo
		use
	endif

	if mfile(w0fipre+'04.dbf')
		ncf04=w0fipre+'04'
		fieldvec[1]='F04C'
		do while .t.
			use (ncf04) in 0 alias 'f04c' EXCLUSIVE
			if !neterr()
				exit
			endif
		enddo
		fieldvec=''
		sele f04c
		pack
		index on d4tipo tag d4tipo
		use
	endif

	wait wind 'Fin del Proceso.'

endif
return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC ftable
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS ft_pref,ft_stat
PRIVATE ALL LIKE ft_*

do case
	case ft_pref=='TC'
		ft_title='TIPOS DE MOVIMIENTOS COMPLEMENTARIOS'
		ft_strc=.f.
		ft_pict='!'
	case ft_pref=='GC'
		ft_title='GRUPOS DE COMPROBANTES'
		ft_strc=.f.
		ft_pict='!'
	case ft_pref=='TD'
		ft_title='TIPOS DE DOCUMENTO'
		ft_strc=.t.
		ft_pict='99'
	case ft_pref=='CO'
		ft_title='TIPOS DE COMPROBANTES'
		ft_strc=.t.
*		ft_pict='99'	&& R.13 Mariano
		ft_pict='999'	&& R.13 Mariano
	&& R.13b Mariano
	case ft_pref=='TO'
		ft_title='TIPOS DE OPERACION'
		ft_strc=.F.
		ft_pict='X'
	&& R.13e Mariano
	case ft_pref=='TI'
		ft_title='TIPOS IMPOSITIVOS'
		ft_strc=.f.
		ft_pict='X'
*????
	case ft_pref=='RP'							&& Compras
		ft_title='RUBROS DE RETENCIONES PRACTICADAS'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RS'							&& Sufridas
		ft_title='RUBROS DE RETENCIONES SUFRIDAS'
		ft_strc=.t.
		ft_pict='999999'

	case ft_pref=='RC'
		ft_title='RUBROS DE COMPRAS'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RCRP'
		ft_title='RUBROS DE COMPRAS Y RETENCIONES'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RV'
		ft_title='RUBROS DE VENTAS'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RVRC'
		ft_title='RUBROS DE VENTAS Y COMPRAS'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RVRS'
		ft_title='RUBROS DE VENTAS Y RETENCIONES'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='RVRCRPRS'
		ft_title='RUBROS DE VENTAS, COMPRAS Y RETENCIONES'
		ft_strc=.t.
		ft_pict='999999'
	case ft_pref=='JD'
		ft_title='JURISDICCIONES'
		ft_strc=.f.
		ft_pict='XX'
	case ft_pref=='JU'
		ft_title='JURISDICCIONES'
		ft_strc=.f.
		ft_pict='XX'
	case ft_pref=='CL'
		ft_title='CLASE DE OPERACION'
		ft_strc=.f.
		ft_pict='!'
	case ft_pref=='PR'
		ft_title='PERCEP./RETENC. IVA'
		ft_strc=.F.
		ft_pict='!'
	case ft_pref=='PG'
		ft_title='PERCEP./RETENC. GANANCIAS'
		ft_strc=.F.
		ft_pict='!'
	case ft_pref=='AD'
		ft_title='ADUANAS'
		ft_strc=.t.
		ft_pict='999'
	case ft_pref=='A'
		ft_title='FACTURAS'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='B'
		ft_title='NOTAS DE DEBITO'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='C'
		ft_title='NOTAS DE CREDITO'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='8'
		ft_title='RECIBO-FACTURA'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='9'
		ft_title='FACTURAS DE CREDITO'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='?'
		ft_title='TICKETS'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='6'
		ft_title='TICKET-FACTURA'
		ft_strc=.t.
		ft_pict='9999'
	&& R.13b Mariano
	case ft_pref=='E'
		ft_title='TICKET-NC'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='D'
		ft_title='TICKET-ND'
		ft_strc=.t.
		ft_pict='9999'
	&& R.13e Mariano
	case ft_pref=='@'
		ft_title='RECIBOS'
		ft_strc=.t.
		ft_pict='9999'
	&& R.18b Mariano
	case ft_pref=='7'
		ft_title='LIQ. VARIAS'
		ft_strc=.t.
		ft_pict='9999'
	case ft_pref=='O'
		ft_title='OTROS'
		ft_strc=.t.
		ft_pict='9999'
	&& R.18e Mariano
	case ft_pref=='R'    
		ft_title='RETENCIONES'
		ft_strc=.t.
		ft_pict='9999'
	otherwise
		ft_title=''
		ft_strc=.f.
		ft_pict=space(20)	&& dejar asi
endcase
do case
	case ft_stat='title'
		ft_retu=ft_title
	case ft_stat='strcero'
		ft_retu=ft_strc
	case ft_stat='picture'
		ft_retu=ft_pict
	case ft_stat='len'
		ft_retu=len(ft_pict)
endcase
return(ft_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC ptoi
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para p_peri
return right(p_peri,4)+left(p_peri,2)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC imco
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

*===>
		&& R.11b Mariano
		if type('d0paco')='U'
			public d0paco,d0cciva,d0juce
			d0paco=''
			d0cciva	= ''					&& Usa Contabilidad IVA
			d0juce	= ''					&&(Jurisdicciขn)
		endif
		&& R.11e Mariano	
		
		w_d0paco	= d0paco
		IF	(d0cciva = "S"	 .AND.	d0paco	= "\CG\")
					w_d0paco	= "CO\"
		ENDIF
		IF	(d0cciva = "N"	 .AND.	d0paco	= "CO\")
					w_d0paco	= "\CG\"
		ENDIF

*===>

return iif(!empty(d0emcg) .and. mfile(w_d0paco+'cof010.dbf').and.!empty(w0fipre),.t.,.f.)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC dicuit
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para _nume
_nume=strtran(_nume,'-','')
if len(_nume)<>10
	return 'X'
endif
_suma=0
_semi="5432765432"
for i=1 to 10
	_suma=_suma+val(substr(_nume,i,1))*val(substr(_semi,i,1))
next
wre=mod(_suma,11)
if wre=0
	_digi="0"
else
	_digi=str(11 - wre,1)
endif
retu(_digi)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Ptoc
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS p_varP
p_punt=at('.',d0picp)
p_punw=0
p_temp=''
do while p_punt<>0
	p_punt=p_punt+p_punw
	p_temp=p_temp+substr(p_varP,p_punw+1,p_punt-1-p_punw)
	p_punw=p_punt
	p_punt=at('.',substr(d0picp,p_punt+1,20))
enddo
Return (subs(p_temp+'                ',1,15))

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Ctop
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS c_varC
c_var='@R '+d0pict
return transform(c_varc,c_var)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC inico
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv walia,wesq,wcant,e_jerc
w0fipre=''
walias=alias()
d0pict=''
d0picp=''
d0niv1=0
d0niv2=0
d0niv3=0
d0niv4=0
d0niv5=0
d0niv6=0


	if !empty(d0emcg)

*===>
		w_d0paco	= d0paco
		IF	(d0cciva = "S"	 .AND.	d0paco	= "\CG\")
					w_d0paco	= "CO\"
		ENDIF
		IF	(d0cciva = "N"	 .AND.	d0paco	= "CO\")
					w_d0paco	= "\CG\"
		ENDIF
		
*		if mfile(d0paco+'cof010.dbf')

		IF mfile(	w_d0paco+'cof010.dbf')
*===>
			sele 0
			*set excl off &&(hago esto para usar el indece de ese directorio)

*===>
*			use (d0paco+'cof010') alia f10c order 'p0coem'
			use (w_d0paco+'cof010') alia f10c order 'p0coem'
*===>
			*set excl on

			if seek(d0emcg,'f10c')

				wesq=trim(f10c->p0path)+'C'+alltrim(f10c->p0coem)+'??00.dbf'
	
				if at(':',wesq)=0
					* Si el drive no esta especificado en la contabilidad
					* y sก lo esta en d0paco agrega el drive segun d0paco

*===>
*					wesq = left(d0paco,at(':',d0paco)) + wesq
					wesq = left(w_d0paco,at(':',w_d0paco)) + wesq
*===>

				endif

				declare w_temp[1]
				w_temp=''

				wcant=adir(w_temp,wesq)

				if empty(wcant)
						sele f10c
						use &&(f10c conta)
						wait wind 'No Encuentra Ejercicios '+wesq
						if !empty(walias)
								sele (walias)
						endif
						return
				endif

				declare e_jerc[wcant]
				=adir(e_jerc,wesq)
				=asort(e_jerc)

				*)--------------------------------------------------------------
				x_tres=subs(e_jerc[1,1],len(e_jerc[1,1])-10,3)  && C00
	
				for i=1 to wcant
						e_jerc[i,1]=subs(e_jerc[i,1],len(e_jerc[i,1])-7,2)
				next

				for i=1 to wcant
						e_jerc[i,1]=iif(val(e_jerc[i,1])>80,'19','20')+e_jerc[i,1]
				next

				=asort(e_jerc)

				e_jerc[wcant,1]=x_tres+subs(e_jerc[wcant,1],3,2)

				*)--------------------------------------------------------------

				w0fipre = alltrim(f10c->p0path)+left(e_jerc[wcant,1],5)

				if at(':',w0fipre)=0
						* Si el drive no esta especificado en la contabilidad
						* y sก lo esta en d0paco agrega el drive segun d0paco

*===>
*						w0fipre = left(d0paco,at(':',d0paco)) + w0fipre
						w0fipre = left(w_d0paco,at(':',w_d0paco)) + w0fipre
*===>

				endif

				sele f10c
				use &&(f10c conta)

				ncf00=w0fipre+'00'
				fieldvec[1]='F00C'
				do while .t.
						use (ncf00) in 0 alias f00c
						if !neterr()
								exit
						endif
				enddo
				fieldvec=''

				d0pict=trim(f00c.p0pict)
				d0picp=''
				for i=1 to len(d0pict)
						if subs(d0pict,i,1)='9'
									d0picp=d0picp+'9'
						else
							d0picp=d0picp+'.'
						endif
				endfor

				d0picp=d0picp+'.' &&(El pict con separador estandard.)
				d0niv1=p0niv1
				d0niv2=p0niv2
				d0niv3=p0niv3
				d0niv4=p0niv4
				d0niv5=p0niv5
				d0niv6=p0niv6

				sele f00c
				use  &&(f00c conta)

			else

				wait wind 'Empresa Contable '+d0emcg+' no existe en '+d0paco
				sele f10c
				use  &&(f10c conta)

			endif
		else
			wait wind 'Existe algun problema con la Empresa Contable'
		endif
	endif

if !empty(walias)
	sele (walias)
endif

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC condi
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameter c_clav
return (c_clav$w0nume)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC t_rans &&(Filtrado de picture numerico)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS t_val,t_tip
PRIVATE t_retu

if parameters()<2
	t_tip= ' '
endif

w_lotus=.f.

if w_lotus &&(es por el tema del punto o la coma)
	t_retu  = str(t_val,17,2)
else
	if empty(t_val) .and. t_tip='B'
		t_retu = spac(17)
	else
		set separ to '.'
		set point to ','
		t_retu  = right(transform(t_val,d0pisa),17)
		set separ to
		set point to
	endif
endif
return(t_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC ctaOk
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Sintaxis        : varL=ctaOk(varC)
*)
*) Servicio        : Testea la buena forma de una cuenta con el picture
*)
*) Par metros
*)            varC : String que expresa un Cขdigo de cuenta con el picture
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS c_cuen
c_cta=trim(Ptoc(c_cuen))
c_len=nivel(c_cta)
if c_len=0
	Return(.F.)
else
	if at(' ',c_cta)<>0
		Return(.F.)
	endif
endif
Return (.T.)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC cpadre
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Sintaxis        : varC = cpadre(varC)
*)
*) Servicio        : Retorna la cuenta padre de una cuenta
*)
*) Par metros
*)            varC : Cขdigo de cuenta sin el picture
*)
*) Observaciones   : El valor de retorno es de 15 char de largo, llenado con
*)                   espacios a la izquierda.
*)                   Si la cuenta es de nivel 1, el retorno son 15 blancos.
*)                   Esta funcion no verifica la existencia de la cta padre
*)                   en el archivo de cuentas.
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS c_cuen
c_niv=str(nivel(c_cuen)-1,1)
if val(c_niv)>0
	c_ret=subs(subst(c_cuen,1,d0niv&c_niv)+spac(15),1,15)
else
	c_ret=space(15)
endif
return(c_ret)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC nivel
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Sintaxis        : varN = nivel(varC)
*)
*) Servicio        : Retorna el nivel de una cuenta
*)
*) Par metros
*)            varC : Cขdigo de cuenta sin el picture
*)
*) Observaciones   : Si no es posible determinar el nivel, esta funcion
*)                   devuelve el valor 0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS n_cuen
n_len=len(trim(n_cuen))
do case
	case n_len=d0niv1
		return 1
	case n_len=d0niv2
		return 2
	case n_len=d0niv3
		return 3
	case n_len=d0niv4
		return 4
	case n_len=d0niv5
		return 5
	case n_len=d0niv6
		return 6
	otherwise
		return 0
endcase
return(0)


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC creatipos
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_clipro

sele 0
create dbf &d0f99.b (tipo c(2), clav c(10), desc c(70), jurafip C(3))
use &d0f99.b alias f01t exclusive	&& R.16 Mariano (agregue exclusive)
index on tipo+clav tag 'clav'
index on tipo+desc tag 'desc'
set order to 'clav'

INSERT INTO f01t VALUES ('TD','80','C.U.I.T.'," ")
INSERT INTO f01t VALUES ('TD','83','IDENTIF. TRIBUTARIA DEL EXTERIOR'," ")
INSERT INTO f01t VALUES ('TD','84','DOCUMENTO DEL EXTERIOR'," ")
INSERT INTO f01t VALUES ('TD','86','C.U.I.L.'," ")
INSERT INTO f01t VALUES ('TD','87','C.D.I.'," ")
INSERT INTO f01t VALUES ('TD','99','SIN IDENTIFICAR VENTA GLOBAL DIARIA'," ")

INSERT INTO f01t VALUES ('TD','89','L.E.'," ")
INSERT INTO f01t VALUES ('TD','90','L.C.'," ")
INSERT INTO f01t VALUES ('TD','91','CI EXTRANJERA'," ")
INSERT INTO f01t VALUES ('TD','92','EN TRAMITE'," ")
INSERT INTO f01t VALUES ('TD','93','ACTA DE NACIMIENTO'," ")
INSERT INTO f01t VALUES ('TD','94','PASAPORTE'," ")
INSERT INTO f01t VALUES ('TD','95','CI BS AS R N P'," ")
INSERT INTO f01t VALUES ('TD','96','D.N.I.'," ")
INSERT INTO f01t VALUES ('TD','00','CI POLICIA FEDERAL'," ")
INSERT INTO f01t VALUES ('TD','01','CI BUENOS AIRES'," ")
INSERT INTO f01t VALUES ('TD','02','CI CATAMARCA'," ")
INSERT INTO f01t VALUES ('TD','03','CI CORDOBA'," ")
INSERT INTO f01t VALUES ('TD','04','CI CORRIENTES'," ")
INSERT INTO f01t VALUES ('TD','05','CI ENTRE RIOS'," ")
INSERT INTO f01t VALUES ('TD','06','CI JUJUY'," ")
INSERT INTO f01t VALUES ('TD','07','CI MENDOZA'," ")
INSERT INTO f01t VALUES ('TD','08','CI LA RIOJA'," ")
INSERT INTO f01t VALUES ('TD','09','CI SALTA'," ")
INSERT INTO f01t VALUES ('TD','10','CI SAN JUAN'," ")
INSERT INTO f01t VALUES ('TD','11','CI SAN LUIS'," ")
INSERT INTO f01t VALUES ('TD','12','CI SANTA FE'," ")
INSERT INTO f01t VALUES ('TD','13','CI STGO. DEL ESTERO'," ")
INSERT INTO f01t VALUES ('TD','14','CI TUCUMAN'," ")
INSERT INTO f01t VALUES ('TD','16','CI CHACO'," ")
INSERT INTO f01t VALUES ('TD','17','CI CHUBUT'," ")
INSERT INTO f01t VALUES ('TD','18','CI FORMOSA'," ")
INSERT INTO f01t VALUES ('TD','19','CI MISIONES'," ")
INSERT INTO f01t VALUES ('TD','20','CI NEUQUEN'," ")
INSERT INTO f01t VALUES ('TD','21','CI LA PAMPA'," ")
INSERT INTO f01t VALUES ('TD','22','CI RIO NEGRO'," ")
INSERT INTO f01t VALUES ('TD','23','CI SANTA CRUZ'," ")
INSERT INTO f01t VALUES ('TD','24','CI TIERRA DEL FUEGO'," ")

INSERT INTO f01t VALUES ('TI','1','Responsable Inscripto'," ")
INSERT INTO f01t VALUES ('TI','2','Responsable No Inscripto'," ")
INSERT INTO f01t VALUES ('TI','3','Consumidor Final'," ")
INSERT INTO f01t VALUES ('TI','4','Exento'," ")
INSERT INTO f01t VALUES ('TI','5','No Responsable'," ")
INSERT INTO f01t VALUES ('TI','6','Responsable Monotributo'," ")
INSERT INTO f01t VALUES ('TI','7','No Categorizado'," ")

*INSERT INTO f01t VALUES ('TI','01','Responsable Inscripto'," ")
*INSERT INTO f01t VALUES ('TI','02','Responsable No Inscripto'," ")
*INSERT INTO f01t VALUES ('TI','05','Consumidor Final'," ")
*INSERT INTO f01t VALUES ('TI','04','Exento'," ")
*INSERT INTO f01t VALUES ('TI','03','No Responsable'," ")
*INSERT INTO f01t VALUES ('TI','06','Responsable Monotributo'," ")
*INSERT INTO f01t VALUES ('TI','07','No Categorizado'," ")
*INSERT INTO f01t VALUES ('TI','08','Proveedor del Exterior'," ")
*INSERT INTO f01t VALUES ('TI','09','Cliente del Exterior'," ")
*INSERT INTO f01t VALUES ('TI','10','IVA Liberado - Ley Nบ 19.640'," ")
*INSERT INTO f01t VALUES ('TI','11','IVA Responsable Inscripto - Agente de percepci๓n'," ")
*INSERT INTO f01t VALUES ('TI','12','Peque๑o Contribuyente Eventual'," ")
*INSERT INTO f01t VALUES ('TI','13','Monotributista Social'," ")
*INSERT INTO f01t VALUES ('TI','14','Peque๑o Contribuyente Eventual Social'," ")

&& R.13b Mariano
INSERT INTO f01t VALUES ('TO','0','NO CORRESPONDE'," ")
INSERT INTO f01t VALUES ('TO','A','NO ALCANZADO'," ")
INSERT INTO f01t VALUES ('TO','C','OPERACIONES DE CANJE'," ")
INSERT INTO f01t VALUES ('TO','E','OPERACIONES EXENTAS'," ")
INSERT INTO f01t VALUES ('TO','N','NO GRAVADO'," ")
INSERT INTO f01t VALUES ('TO','X','EXP/IMP.DEL EXTERIOR'," ")
INSERT INTO f01t VALUES ('TO','Z','EXP/IMP.DE Z. FRANCA'," ")
&& R.13e Mariano

INSERT INTO f01t VALUES ('CO','001','Factura A          '," ")
INSERT INTO f01t VALUES ('CO','002','Nota de D้bito A   '," ")
INSERT INTO f01t VALUES ('CO','003','Nota de Cr้dito A  '," ")
INSERT INTO f01t VALUES ('CO','004','Recibo A           '," ")
INSERT INTO f01t VALUES ('CO','005','Notas de vta.ctdo.A'," ")
INSERT INTO f01t VALUES ('CO','006','Factura B          '," ")
INSERT INTO f01t VALUES ('CO','007','Nota de D้bito B   '," ")
INSERT INTO f01t VALUES ('CO','008','Nota de Cr้dito B  '," ")
INSERT INTO f01t VALUES ('CO','009','Recibo B           '," ")
INSERT INTO f01t VALUES ('CO','010','Notas de vta.ctdo.B'," ")
INSERT INTO f01t VALUES ('CO','011','Factura C          '," ")
INSERT INTO f01t VALUES ('CO','012','Nota de D้bito C   '," ")
INSERT INTO f01t VALUES ('CO','013','Nota de Cr้dito C  '," ")
INSERT INTO f01t VALUES ('CO','014','Documento Aduanero '," ")	
INSERT INTO f01t VALUES ('CO','015','Recibo C           '," ")
INSERT INTO f01t VALUES ('CO','016','Notas de vta.ctdo.C'," ")
INSERT INTO f01t VALUES ('CO','017','Liq.serv.publicos A'," ")	&& R.12 Mariano
INSERT INTO f01t VALUES ('CO','018','Liq.serv.publicos B'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','019','Facturas de Export.'," ")
INSERT INTO f01t VALUES ('CO','020','Nota de d้bito Op.E'," ")
INSERT INTO f01t VALUES ('CO','021','Nota de cr้dito O.E'," ")
INSERT INTO f01t VALUES ('CO','022','Fac.P.Ex.DTO.855 97'," ")
INSERT INTO f01t VALUES ('CO','023','Cptes"A" cpra.prim.'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','024','Cptes"A" consig.pr.'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','025','Cptes"B" cpra.prim.'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','026','Cptes"B" consig.pr.'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','027','Liq.unica com.imp.A'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','028','Liq.unica com.imp.B'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','029','Liq.unica com.imp.C'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','030','Compras bien.usados'," ")
INSERT INTO f01t VALUES ('CO','032','Cptes p/recicl.mat.'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','033','Liq.prim. de granos'," ")	&& R.13 Mariano
*INSERT INTO f01t VALUES ('CO','34','A art.3 inc.F 1415 '," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','35','B art.3 inc.F 1415 '," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','36','C art.3 inc.F 1415 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','034','A art.3 inc.E 3419 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','035','B art.3 inc.E 3419 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','036','C art.3 inc.E 3419 '," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','37','Nd o Doc.Equiv.1415'," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','38','Nc o Doc.Equiv.1415'," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','39','Otro A Cumple 1415 '," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','40','Otro B Cumple 1415 '," ")	&& R.07 Mariano
*INSERT INTO f01t VALUES ('CO','41','Otro C Cumple 1415 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','037','Nd o Doc.Equiv.3419'," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','038','Nc o Doc.Equiv.3419'," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','039','Otro A Cumple 3419 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','040','Otro B Cumple 3419 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','041','Otro C Cumple 3419 '," ")	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','042','Otro Cumple 3419(-)','')	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','043','NC liq.unica com. B','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','044','NC liq.unica com. C','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','045','ND liq.unica com. A','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','046','ND liq.unica com. B','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','047','ND liq.unica com. C','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','048','NC liq.unica com. A','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','049','Cpra.bnes.no reg.CF','')	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','050','Rec.Fact.A (F.cred)','')	&& R.07 Mariano
INSERT INTO f01t VALUES ('CO','051','Facturas M         '," ")
INSERT INTO f01t VALUES ('CO','052','Nota de d้bito M   '," ")
INSERT INTO f01t VALUES ('CO','053','Nota de cr้dito M  '," ")
INSERT INTO f01t VALUES ('CO','054','Recibos M          '," ")
INSERT INTO f01t VALUES ('CO','055','Nota de Vta.Ctdo M '," ")
INSERT INTO f01t VALUES ('CO','056','M An.I Ap.A In.F 1415'," ")
INSERT INTO f01t VALUES ('CO','057','Otro M Cumple 1415  '," ")
INSERT INTO f01t VALUES ('CO','058','Ctas.Vtas.-liq.pr.M '," ")
INSERT INTO f01t VALUES ('CO','059','Liquidaci๓n M       '," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','060','Ctas.vtas.-liq.pr.A'," ")
INSERT INTO f01t VALUES ('CO','061','Ctas.vtas.-liq.pr.B'," ")
INSERT INTO f01t VALUES ('CO','062','Ctas.vtas.-liq.pr.C'," ")
INSERT INTO f01t VALUES ('CO','063','Liquidaciones A    '," ")
INSERT INTO f01t VALUES ('CO','064','Liquidaciones B    '," ")
*INSERT INTO f01t VALUES ('CO','65','Liquidaciones C    '," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','066','Desp.de importaci๓n'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','068','Liquidaciones C    '," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('CO','070','Rec.de fact.Cr้d.R '," ")
INSERT INTO f01t VALUES ('CO','080','Comp.Diar.Cierre Z '," ")
INSERT INTO f01t VALUES ('CO','081','Tique Factura A    '," ")
INSERT INTO f01t VALUES ('CO','082','Tique Factura B    '," ")
INSERT INTO f01t VALUES ('CO','083','Tique              '," ")
INSERT INTO f01t VALUES ('CO','084','Comp.Fact.Serv.Publ'," ")
INSERT INTO f01t VALUES ('CO','085','NC Serv.Publicos   '," ")
INSERT INTO f01t VALUES ('CO','086','ND Serv.Publicos   '," ")
INSERT INTO f01t VALUES ('CO','087','Otro Comp.-Serv.ext'," ")
INSERT INTO f01t VALUES ('CO','088','Otro Comp.-Doc.exc.'," ")
INSERT INTO f01t VALUES ('CO','089','Otro Comp.-D.e.N.D.'," ")
INSERT INTO f01t VALUES ('CO','090','Otro Comp.-D.e.N.C.'," ")
INSERT INTO f01t VALUES ('CO','092','Ajuste ctble.+ D.F.'," ")
INSERT INTO f01t VALUES ('CO','093','Ajuste ctble.- D.F.'," ")
INSERT INTO f01t VALUES ('CO','094','Ajuste ctble.+ C.F.'," ")
INSERT INTO f01t VALUES ('CO','095','Ajuste ctble.- C.F.'," ")
INSERT INTO f01t VALUES ('CO','096','Formulario 1116/B  '," ")
INSERT INTO f01t VALUES ('CO','097','Formulario 1116/C  '," ")
INSERT INTO f01t VALUES ('CO','099','Otro No Cumple 3419'," ")
&& R.13b Mariano
INSERT INTO f01t VALUES ('CO','110','Ticket NC           ','')
INSERT INTO f01t VALUES ('CO','111','Ticket FACTURA C    ','')
INSERT INTO f01t VALUES ('CO','112','Ticket NC A         ','')
INSERT INTO f01t VALUES ('CO','113','Ticket NC B         ','')
INSERT INTO f01t VALUES ('CO','114','Ticket NC C         ','')
INSERT INTO f01t VALUES ('CO','115','Ticket ND A         ','')
INSERT INTO f01t VALUES ('CO','116','Ticket ND B         ','')
INSERT INTO f01t VALUES ('CO','117','Ticket ND C         ','')
INSERT INTO f01t VALUES ('CO','118','Ticket FACTURA M    ','')
INSERT INTO f01t VALUES ('CO','119','Ticket NC M         ','')
INSERT INTO f01t VALUES ('CO','120','Ticket ND M         ','')
&& R.22b Mariano
INSERT INTO f01t VALUES ('CO','150','Liq.cpr.prim.p/TABAC','')
INSERT INTO f01t VALUES ('CO','151','Liq.cpr.prim.p/TABAC','')
INSERT INTO f01t VALUES ('CO','180','Cta.Vta.Liq.Prod.A P','')
INSERT INTO f01t VALUES ('CO','182','Cta.Vta.Liq.Prod.B P','')
INSERT INTO f01t VALUES ('CO','183','Liq.Cpra.A PECUARIO','')
INSERT INTO f01t VALUES ('CO','185','Liq.Cpra.B PECUARIO','')
INSERT INTO f01t VALUES ('CO','186','Liq.Cpra.Dcta.A PEC','')
INSERT INTO f01t VALUES ('CO','188','Liq.Cpra.Dcta.B PEC','')
INSERT INTO f01t VALUES ('CO','189','Liq.Cpra.Dcta.C PEC','')
INSERT INTO f01t VALUES ('CO','190','Liq.Vta. Dcta.A PEC','')
INSERT INTO f01t VALUES ('CO','191','Liq.Vta. Dcta.B PEC','')
INSERT INTO f01t VALUES ('CO','195','Factura Clase "T"  ','')
INSERT INTO f01t VALUES ('CO','196','N.D้bito Clase "T" ','')
INSERT INTO f01t VALUES ('CO','197','N.Cr้dito Clase "T"','')
&& R.22e Mariano
INSERT INTO f01t VALUES ('CO','331','Liq.secund.de granos','')
INSERT INTO f01t VALUES ('CO','332','Cert.dep.granos plta','')
&& R.13e Mariano

INSERT INTO f01t VALUES ('CL','1','Bienes de Uso'," ")
INSERT INTO f01t VALUES ('CL','2','Locaciones'," ")
INSERT INTO f01t VALUES ('CL','3','Anexo I'," ")
INSERT INTO f01t VALUES ('CL','A','Compras de Bienes en el Mercado Local (Excepto Bienes de Uso)'," ")	&& R.21 Mariano (Saque (1) de la descripcion)
INSERT INTO f01t VALUES ('CL','B','Compras de Bienes Usados a Consumidores Finales'," ")	&& R.21 Mariano (Saque (1) de la descripcion)
INSERT INTO f01t VALUES ('CL','C','Inversiones en Bienes de Uso'," ")	&& R.21 Mariano (Saque (1) de la descripcion)
INSERT INTO f01t VALUES ('CL','D','Ventas de Bienes de Uso Propios'," ")	&& R.21 Mariano (Saque (1) de la descripcion)
INSERT INTO f01t VALUES ('CL','E','Ventas de Bienes de Cambio'," ")	&& R.21 Mariano (Saque (1) de la descripcion)
INSERT INTO f01t VALUES ('CL','F','Prestaci๓n de Servicios'," ")	&& R.21 Mariano (Saque (2) de la descripcion)
INSERT INTO f01t VALUES ('CL','G','Otros Conceptos'," ")	&& R.21 Mariano (Saque (3) de la descripcion)
INSERT INTO f01t VALUES ('CL','H','Tur IVA'," ")	&& R.21 Mariano
INSERT INTO f01t VALUES ('CL','I','Contribuciones Seguridad Social'," ")	&& R.21 Mariano

INSERT INTO f01t VALUES ('UM','01','Kilogramo'," ")
INSERT INTO f01t VALUES ('UM','02','Metros'," ")
INSERT INTO f01t VALUES ('UM','03','Metro Cuadrado'," ")
INSERT INTO f01t VALUES ('UM','04','Metro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','05','Litros'," ")
INSERT INTO f01t VALUES ('UM','06','1000 Kilowatt hora'," ")
INSERT INTO f01t VALUES ('UM','07','Unidad'," ")
INSERT INTO f01t VALUES ('UM','08','Par'," ")
INSERT INTO f01t VALUES ('UM','09','Docena'," ")
INSERT INTO f01t VALUES ('UM','10','Quilate'," ")
INSERT INTO f01t VALUES ('UM','11','Millar'," ")
INSERT INTO f01t VALUES ('UM','12','Mega-U. Int. Act. Antib'," ")
INSERT INTO f01t VALUES ('UM','13','Unidad Int. Act. Inmung'," ")
INSERT INTO f01t VALUES ('UM','14','Gramo'," ")
INSERT INTO f01t VALUES ('UM','15','Milํmetro'," ")
INSERT INTO f01t VALUES ('UM','16','Milํmetro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','17','Kil๓metro'," ")
INSERT INTO f01t VALUES ('UM','18','Hectolitro'," ")
INSERT INTO f01t VALUES ('UM','19','Mega U. Int. Act. Inmung'," ")
INSERT INTO f01t VALUES ('UM','20','Centํmetro'," ")
INSERT INTO f01t VALUES ('UM','21','Kilogramo Activo'," ")
INSERT INTO f01t VALUES ('UM','22','Gramo Activo'," ")
INSERT INTO f01t VALUES ('UM','23','Gramo Base'," ")
INSERT INTO f01t VALUES ('UM','24','Uiacthor'," ")
INSERT INTO f01t VALUES ('UM','25','Juego o Paquete Mazo de Naipes'," ")
INSERT INTO f01t VALUES ('UM','26','Muiacthor'," ")
INSERT INTO f01t VALUES ('UM','27','Centํmetro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','28','Uiactant'," ")
INSERT INTO f01t VALUES ('UM','29','Tonelada'," ")
INSERT INTO f01t VALUES ('UM','30','Decแmetro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','31','Hect๓metro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','32','Kil๓metro C๚bico'," ")
INSERT INTO f01t VALUES ('UM','33','Microgramo'," ")
INSERT INTO f01t VALUES ('UM','34','Nanogramo'," ")
INSERT INTO f01t VALUES ('UM','35','Picogramo'," ")
INSERT INTO f01t VALUES ('UM','36','Muiactant'," ")
INSERT INTO f01t VALUES ('UM','37','Uiactig'," ")
INSERT INTO f01t VALUES ('UM','41','Miligramo'," ")
INSERT INTO f01t VALUES ('UM','47','Mililitro'," ")
INSERT INTO f01t VALUES ('UM','48','Curie'," ")
INSERT INTO f01t VALUES ('UM','49','Milicurie'," ")
INSERT INTO f01t VALUES ('UM','50','Microcurie'," ")
INSERT INTO f01t VALUES ('UM','51','U. Inter. Act. Hor.'," ")
INSERT INTO f01t VALUES ('UM','52','Mega U. Inter. Act. Hor.'," ")
INSERT INTO f01t VALUES ('UM','53','Kilogramo Base'," ")
INSERT INTO f01t VALUES ('UM','54','Gruesa'," ")
INSERT INTO f01t VALUES ('UM','55','Muiactig'," ")
INSERT INTO f01t VALUES ('UM','61','Kg. Bruto'," ")
INSERT INTO f01t VALUES ('UM','62','Pack'," ")
INSERT INTO f01t VALUES ('UM','63','Horma'," ")
INSERT INTO f01t VALUES ('UM','98','Otras Unidades'," ")
INSERT INTO f01t VALUES ('UM','99','Bonificaci๓n'," ")


INSERT INTO f01t VALUES ('MO','002','DOLAR EEUU LIBRE'," ")
INSERT INTO f01t VALUES ('MO','003','FRANCOS FRANCESES'," ")
INSERT INTO f01t VALUES ('MO','004','LIRAS ITALIANAS'," ")
INSERT INTO f01t VALUES ('MO','005','PESETAS'," ")
INSERT INTO f01t VALUES ('MO','006','MARCOS ALEMANES'," ")
INSERT INTO f01t VALUES ('MO','007','FLORINES HOLANDESES'," ")
INSERT INTO f01t VALUES ('MO','008','FRANCOS BELGAS'," ")
INSERT INTO f01t VALUES ('MO','009','FRANCOS SUIZOS'," ")
INSERT INTO f01t VALUES ('MO','010','PESOS MEXICANOS'," ")
INSERT INTO f01t VALUES ('MO','011','PESOS URUGUAYOS'," ")
INSERT INTO f01t VALUES ('MO','012','REAL'," ")
INSERT INTO f01t VALUES ('MO','013','ESCUDOS PORTUGUESES'," ")
INSERT INTO f01t VALUES ('MO','014','CORONAS DANESAS'," ")
INSERT INTO f01t VALUES ('MO','015','CORONAS NORUEGAS'," ")
INSERT INTO f01t VALUES ('MO','016','CORONAS SUECAS'," ")
INSERT INTO f01t VALUES ('MO','017','CHELINES AUSTRIACOS'," ")
INSERT INTO f01t VALUES ('MO','018','DOLAR CANADIENSE'," ")
INSERT INTO f01t VALUES ('MO','019','YENS'," ")
INSERT INTO f01t VALUES ('MO','021','LIBRA ESTERLINA'," ")
INSERT INTO f01t VALUES ('MO','022','MARCOS FILANDESES'," ")
INSERT INTO f01t VALUES ('MO','023','BOLIVAR'," ")
INSERT INTO f01t VALUES ('MO','024','CORONA CHECA'," ")
INSERT INTO f01t VALUES ('MO','025','DINAR'," ")
INSERT INTO f01t VALUES ('MO','026','DOLAR AUSTRALIANO'," ")
INSERT INTO f01t VALUES ('MO','027','DRACMA'," ")
INSERT INTO f01t VALUES ('MO','028','FLORIN (ANTILLAS HOL.'," ")
INSERT INTO f01t VALUES ('MO','029','GUARANI'," ")
INSERT INTO f01t VALUES ('MO','030','SHEKEL (ISRAEL)'," ")
INSERT INTO f01t VALUES ('MO','031','PESO BOLIVIANO'," ")
INSERT INTO f01t VALUES ('MO','032','PESO COLOMBIANO'," ")
INSERT INTO f01t VALUES ('MO','033','PESO CHILENO'," ")
INSERT INTO f01t VALUES ('MO','034','RAND'," ")
INSERT INTO f01t VALUES ('MO','035','NUEVO SOL PERUANO'," ")
INSERT INTO f01t VALUES ('MO','036','SUCRE'," ")
INSERT INTO f01t VALUES ('MO','050','LIBRAS IRLANDESAS'," ")
INSERT INTO f01t VALUES ('MO','051','DOLAR DE HONG KONG'," ")
INSERT INTO f01t VALUES ('MO','052','DOLAR DE SINGAPUR'," ")
INSERT INTO f01t VALUES ('MO','053','DOLAR DE JAMAICA'," ")
INSERT INTO f01t VALUES ('MO','054','DOLAR DE TAIWAN'," ")
INSERT INTO f01t VALUES ('MO','055','QUETZAL'," ")
INSERT INTO f01t VALUES ('MO','056','FORINT (HUNGRIA)'," ")
INSERT INTO f01t VALUES ('MO','057','BAHT (TAILANDIA)'," ")
INSERT INTO f01t VALUES ('MO','058','ECU'," ")
INSERT INTO f01t VALUES ('MO','059','DINAR KUWAITI'," ")
INSERT INTO f01t VALUES ('MO','060','EURO'," ")
INSERT INTO f01t VALUES ('MO','DOL','DOLAR ESTADOUNIDENSE'," ")
INSERT INTO f01t VALUES ('MO','PES','PESOS'," ")

INSERT INTO f01t VALUES ('JD','00','Capital Federal',"901")
INSERT INTO f01t VALUES ('JD','01','Buenos Aires',"902")
INSERT INTO f01t VALUES ('JD','02','Catamarca',"903")
INSERT INTO f01t VALUES ('JD','03','C๓rdoba',"904")
INSERT INTO f01t VALUES ('JD','04','Corrientes',"905")
INSERT INTO f01t VALUES ('JD','05','Entre Rํos',"908")
INSERT INTO f01t VALUES ('JD','06','Jujuy',"910")
INSERT INTO f01t VALUES ('JD','07','Mendoza',"913")
INSERT INTO f01t VALUES ('JD','08','La Rioja',"912")
INSERT INTO f01t VALUES ('JD','09','Salta',"917")
INSERT INTO f01t VALUES ('JD','10','San Juan',"918")
INSERT INTO f01t VALUES ('JD','11','San Luis',"919")
INSERT INTO f01t VALUES ('JD','12','Santa Fe',"921")
INSERT INTO f01t VALUES ('JD','13','Santiago del Estero',"922")
INSERT INTO f01t VALUES ('JD','14','Tucumแn',"924")
INSERT INTO f01t VALUES ('JD','16','Chaco',"906")
INSERT INTO f01t VALUES ('JD','17','Chubut',"907")
INSERT INTO f01t VALUES ('JD','18','Formosa',"909")
INSERT INTO f01t VALUES ('JD','19','Misiones',"914")
INSERT INTO f01t VALUES ('JD','20','Neuqu้n',"915")
INSERT INTO f01t VALUES ('JD','21','La Pampa',"911")
INSERT INTO f01t VALUES ('JD','22','Rํo Negro',"916")
INSERT INTO f01t VALUES ('JD','23','Santa Cruz',"920")
INSERT INTO f01t VALUES ('JD','24','Tierra del Fuego',"923")
INSERT INTO f01t VALUES ('JD','99','No se informa Prov.'," ")

INSERT INTO f01t VALUES ('AD','000','ADMINISTRACION CENTRAL'," ")
INSERT INTO f01t VALUES ('AD','001','BUENOS AIRES CAPITAL'," ")
INSERT INTO f01t VALUES ('AD','003','BAHIA BLANCA'," ")
INSERT INTO f01t VALUES ('AD','004','BARILOCHE'," ")
INSERT INTO f01t VALUES ('AD','008','CAMPANA'," ")
INSERT INTO f01t VALUES ('AD','010','BARRANQUERAS'," ")
INSERT INTO f01t VALUES ('AD','012','CLORINDA'," ")
INSERT INTO f01t VALUES ('AD','013','COLON'," ")
INSERT INTO f01t VALUES ('AD','014','COMODORO RIVADAVIA'," ")
INSERT INTO f01t VALUES ('AD','015','CONCEPCION DEL URUGUAY'," ")
INSERT INTO f01t VALUES ('AD','016','CONCORDIA'," ")
INSERT INTO f01t VALUES ('AD','017','CORDOBA'," ")
INSERT INTO f01t VALUES ('AD','018','CORRIENTES'," ")
INSERT INTO f01t VALUES ('AD','019','PUERTO DESEADO'," ")
INSERT INTO f01t VALUES ('AD','020','DIAMANTE'," ")
INSERT INTO f01t VALUES ('AD','023','ESQUEL'," ")
INSERT INTO f01t VALUES ('AD','024','FORMOSA'," ")
INSERT INTO f01t VALUES ('AD','025','GOYA'," ")
INSERT INTO f01t VALUES ('AD','026','GUALEGUAYCHU'," ")
INSERT INTO f01t VALUES ('AD','029','IGUAZU'," ")
INSERT INTO f01t VALUES ('AD','031','JUJUY'," ")
INSERT INTO f01t VALUES ('AD','033','LA PLATA'," ")
INSERT INTO f01t VALUES ('AD','034','LA QUIACA'," ")
INSERT INTO f01t VALUES ('AD','037','MAR DEL PLATA'," ")
INSERT INTO f01t VALUES ('AD','038','MENDOZA'," ")
INSERT INTO f01t VALUES ('AD','040','NECOCHEA'," ")
INSERT INTO f01t VALUES ('AD','041','PARANA'," ")
INSERT INTO f01t VALUES ('AD','042','PASO DE LOS LIBRES'," ")
INSERT INTO f01t VALUES ('AD','045','POCITOS'," ")
INSERT INTO f01t VALUES ('AD','046','POSADAS'," ")
INSERT INTO f01t VALUES ('AD','047','PUERTO MADRYN'," ")
INSERT INTO f01t VALUES ('AD','048','RIO GALLEGOS'," ")
INSERT INTO f01t VALUES ('AD','049','RIO GRANDE'," ")
INSERT INTO f01t VALUES ('AD','052','ROSARIO'," ")
INSERT INTO f01t VALUES ('AD','053','SALTA'," ")
INSERT INTO f01t VALUES ('AD','054','SAN JAVIER'," ")
INSERT INTO f01t VALUES ('AD','055','SAN JUAN'," ")
INSERT INTO f01t VALUES ('AD','057','SAN LORENZO'," ")
INSERT INTO f01t VALUES ('AD','058','SAN MARTIN DE LOS ANDES'," ")
INSERT INTO f01t VALUES ('AD','059','SAN NICOLAS'," ")
INSERT INTO f01t VALUES ('AD','060','SAN PEDRO'," ")
INSERT INTO f01t VALUES ('AD','061','SANTA CRUZ'," ")
INSERT INTO f01t VALUES ('AD','062','SANTA FE'," ")
INSERT INTO f01t VALUES ('AD','065','TIGRE'," ")
INSERT INTO f01t VALUES ('AD','066','TINOGASTA'," ")
INSERT INTO f01t VALUES ('AD','067','USHUAIA'," ")
INSERT INTO f01t VALUES ('AD','069','VILLA CONSTITUCION'," ")
INSERT INTO f01t VALUES ('AD','073','EZEIZA'," ")
INSERT INTO f01t VALUES ('AD','074','TUCUMAN'," ")
INSERT INTO f01t VALUES ('AD','075','NEUQUEN'," ")
INSERT INTO f01t VALUES ('AD','076','ORAN'," ")
INSERT INTO f01t VALUES ('AD','078','SAN RAFAEL'," ")
INSERT INTO f01t VALUES ('AD','079','LA RIOJA'," ")
INSERT INTO f01t VALUES ('AD','080','SAN ANTONIO OESTE'," ")
INSERT INTO f01t VALUES ('AD','082','BERNARDO DE IRIGOYEN'," ")
INSERT INTO f01t VALUES ('AD','083','SAN LUIS'," ")
INSERT INTO f01t VALUES ('AD','084','SANTO TOME'," ")
INSERT INTO f01t VALUES ('AD','085','VILLA REGINA'," ")
&& R.13b Mariano
INSERT INTO f01t VALUES ('AD','086','OBERA'," ")
INSERT INTO f01t VALUES ('AD','087','CALETA OLIVIA'," ")
INSERT INTO f01t VALUES ('AD','088','GENERAL DEHEZA'," ")
INSERT INTO f01t VALUES ('AD','089','SANTIAGO DEL ESTERO'," ")
INSERT INTO f01t VALUES ('AD','090','GENERAL PICO'," ")
INSERT INTO f01t VALUES ('AD','091','BUENOS AIRES NORTE'," ")
INSERT INTO f01t VALUES ('AD','092','BUENOS AIRES SUR'," ")
INSERT INTO f01t VALUES ('AD','266','ZF CORONEL ROSALES'," ")
INSERT INTO f01t VALUES ('AD','267','ZF CONCEPCION DEL URUGUAY'," ")
INSERT INTO f01t VALUES ('AD','999','ADUANA EXCEPCION'," ")
&& R.13e Mariano

INSERT INTO f01t VALUES ('DT','CORI','DECLARACION DE CORRECCION DE INGRESO A DEPOSITO'," ")
INSERT INTO f01t VALUES ('DT','DIS1','DESTINACION DE IMPORTACION SIMPLIFICADA S/DOC'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','DIS4','DESTINACION DE IMPORTACION SIMPLIFICADA'," ")
INSERT INTO f01t VALUES ('DT','DIS5','DESTINACION DE IMPORTACION SIMPLIFICADA DAP'," ")
INSERT INTO f01t VALUES ('DT','EC01','EXPORTACION A CONSUMO'," ")
INSERT INTO f01t VALUES ('DT','EC02','EXPORTACION A CONSUMO C/DIT SIN TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','EC03','EXPORTACION A CONSUMO C/DIT CON TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','EC04','EXPORT. A CONS. C/DIT INGRES. P/TRANSF. EGRESADO S/TRANSF.'," ")
INSERT INTO f01t VALUES ('DT','EC05','EXPORTACION A CONSUMO DE EXPORT. TEMP. C/TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','EC06','EXPORTACION A CONSUMO DE EXPORT. TEMP. S/TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','EC07','EXPORTACION A CONSUMO DE MERCADERIA EN CONSIGNACION'," ")
INSERT INTO f01t VALUES ('DT','EC08','EXPORTACION A CONSUMO DE MERC. EN CONSIGNACION'," ")
INSERT INTO f01t VALUES ('DT','EC09','EXPORTACION A CONSUMO DE CONCENTRADO DE MINERALES'," ")
INSERT INTO f01t VALUES ('DT','EC16','RESIDUOS DE IMPORTACIONES TEMPORARIAS PARA TRANSFORMACION'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','EC19','EXPORTACION A CONSUMO CON CANC DE INSUMOS INGRESADOS TEMP A ZF'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','ECA1','EGRESO A CONS. EN TNC MERC. ORIG. AAE EGRE. MISMO PRODUCTOR'," ")
INSERT INTO f01t VALUES ('DT','ECA2','EGRESO AL TNC DE MERC. ORIG. AAE E IMP CUIT DISTINTO PROD.'," ")
INSERT INTO f01t VALUES ('DT','ECA3','EGRESO P/CONS. EN TNC MERC. DEL AAE NO ORIG. EN EL AREA'," ")
INSERT INTO f01t VALUES ('DT','ECA4','REIMPOR. AL TNC MERC. EXPORT. DESDE TNC AL AAE'," ")
INSERT INTO f01t VALUES ('DT','ECE1','EGRESO DEL AAE A CONS. EN EXT. C/TRANS. TERRES. POR TNC'," ")
&& R.13b Mariano
INSERT INTO f01t VALUES ('DT','ECP1','EGRESO DEF MERCA EXTRANJ C PREV EGR TEMP A PLAT MARINAS'," ")
INSERT INTO f01t VALUES ('DT','ECR1','EXPORTACION A CONSUMO DE BIENES TRANSFORMADOS EN EL RAF'," ")
INSERT INTO f01t VALUES ('DT','ECR2','EXPORTACION A CONSUMO DE BIENES SIN TRANSFORMAR POR VENTA AL EXTERIOR'," ")
INSERT INTO f01t VALUES ('DT','EG01','EXPORTACION A CONSUMO GRAN OPERADOR'," ")
INSERT INTO f01t VALUES ('DT','EG02','EXPORTACION A CONSUMO C DIT SIN TRANS GRAN OPERADOR'," ")
INSERT INTO f01t VALUES ('DT','EG03','EXPORTACION A CONSUMO C DIT CON TRANS GRAN OPERADOR'," ")
INSERT INTO f01t VALUES ('DT','EG06','EXPORTACION A CONSUMO DE EXP TEMP S TRANSF P GRAN OPER EXPORT A CONSUMO DE EX TEMP S TRAN'," ")
INSERT INTO f01t VALUES ('DT','EG07','EXPORTACION A CONSUMO DE MERC EN CONSIG P GRAN OPERADOR'," ")
INSERT INTO f01t VALUES ('DT','EG13','EXPORTACION A CONSUMO C DIT C T GOP CON AUTORIZACION'," ")
&& R.13e Mariano
INSERT INTO f01t VALUES ('DT','ER01','EXPORT. RANCHO COMBUSTIBLE DE BUQUE BANDERA EXTRANJERA'," ")
INSERT INTO f01t VALUES ('DT','ER02','EXPORT. RANCHO P MEDIOS DE TRANSPORTE DE BANDERA NACIONAL'," ")
INSERT INTO f01t VALUES ('DT','ER03','EXPORT. RANCHO Y APROVISIONAM. DE MEDIOS DE TRANSPORTE DE BANDERA EXTRANJ.EXCEPTO COMBS.Y LUBRICANTES'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','ER04','EXPORT. RANCHO Y APROVISIONAM. DE MEDIOS DE TRANSPORTE DE BANDERA NAC EXCEPTO COMBS.Y LUBRICANTES'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','ES01','EXPORTACION DE MERCADERIAS EN CONSIGNACION'," ")
INSERT INTO f01t VALUES ('DT','ES02','EXPORTACION DE MERCADERIAS CON PRECIOS REVISABLES'," ")
INSERT INTO f01t VALUES ('DT','ES03','EXPORTACION DE CONCENTRADO DE MINERALES'," ")
INSERT INTO f01t VALUES ('DT','ET01','EXPORTACION TEMPORARIA CON TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','ET02','EXPORTACION TEMPORARIA SIN TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','ETE1','EGRESO TEMP. DEL AAE AL EXT. C/TRANS. TERRES. POR TNC'," ")
INSERT INTO f01t VALUES ('DT','EXRS','EXPORTACION REGIMEN SIMPLIFICADO DECRETO NRO. 855/97'," ")
INSERT INTO f01t VALUES ('DT','IC01','IMPORTACION A CONSUMO SIN DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC02','IMPORTAC. A CONSUMO MERCADERIA REGIMEN DE SOLICITUD PREVIA'," ")
INSERT INTO f01t VALUES ('DT','IC03','IMPORT. CONSUMO DE MERC. VIA TERREST. ENVIOS FRACC.'," ")
INSERT INTO f01t VALUES ('DT','IC04','IMPORTACION A CONSUMO CON DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC05','IMPORTACION A CONSUMO C/DOC. DE TRANSPORTE D.A.P.'," ")
INSERT INTO f01t VALUES ('DT','IC06','IMPORTACION A CONSUMO SOBRE DEPOSITO DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IC07','IMPORTACION A CONSUMO REGIMEN AUTOMOTOR C/DOC DAP'," ")
INSERT INTO f01t VALUES ('DT','IC08','IMPORTACION A CONSUMO ADUANA DOMICILIARIA C/DOC DAP'," ")
INSERT INTO f01t VALUES ('DT','IC21','RETORNO EXPORT. TEMP. C/TRANSFORMACION S/DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC24','RETORNO EXPORT. TEMPOR. C/TRANSFORM. C/DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC25','RETORNO EXPORT. TEMPOR. C/TRANSFORM. C/DOC. TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','IC26','RETORNO EXPORT. TEMPOR. C/TRANSFORM. S/DEP. DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IC31','RETORNO EXPORT. TEMP. S/TRANSFORM. S/DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC34','RETORNO EXPORT. TEMPOR. S/TRANSFORM. C/DOC. DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IC35','RETORNO EXPORT. TEMPOR. S/TRANSF. C/DOC. DE TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','IC36','RETORNO EXPORT. TEMPOR. S/TRANSFORM. SOBRE DEP. ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IC44','RETORNO EXPORT. EN CONSIGNACION C/DOC. TRANSP.'," ")
INSERT INTO f01t VALUES ('DT','IC45','RETORNO EXPORT. EN CONSIGNACION C/DOC. TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','IC65','IMPORTACION A CONSUMO DE GRANEL DAP CON MANI REGISTRADO'," ")		&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','IC75','IMPORTACION A CONSUMO C DOC TRANSP DAP FORM 7366 MANI REG'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','IC81','IMPORTACION A CONSUMO DE IMPORTACION TEMP. CON TRASNFORMAC.'," ")
INSERT INTO f01t VALUES ('DT','IC82','IMPORTACION A CONSUMO DE RESIDUOS DE IMPORTACION TEMP. C/TRANSF.'," ")
INSERT INTO f01t VALUES ('DT','IC83','IMPORTACION A CONSUMO DE IMPORTACION TEMPORARIA SIN TRANSF.'," ")
&& R.13b Mariano
INSERT INTO f01t VALUES ('DT','IC99','IMPORTACION A CONSUMO DE MERC EN SITUACION DE REZAGO P TITULAR'," ")
INSERT INTO f01t VALUES ('DT','ICR1','IMPORTACION A CONSUMO DE BIENES TRANSFORMADOS RAF'," ")
INSERT INTO f01t VALUES ('DT','ICR2','IMPORTACION A CONSUMO DE BIENES NO TRANSFORMADOS RAF'," ")
INSERT INTO f01t VALUES ('DT','ICR3','IMP A CONSUMO DE BIENES TRANSFORMADOS EGRESADOS DE LA PLANTA CON TLR1'," ")
INSERT INTO f01t VALUES ('DT','ICR4','IMP A CONSUMO DE LOS RESIDUOS DE LOS PROCESOS PRODUCTIVOS EN EL RAF'," ")
&& R.13e Mariano
INSERT INTO f01t VALUES ('DT','IDA2','IMPORTACION SUSPENSIVA DE DEP. DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IDTL','DECLARAC. PARA EL FRACC. DE LOS BULTOS DE UN DOC. TRANSP.'," ")
INSERT INTO f01t VALUES ('DT','IG01','IMPORTACION A CONSUMO P/GRAN. OPERADORES S/DOC. TRANSP.'," ")
INSERT INTO f01t VALUES ('DT','IG03','IMPORT. A CONSUMO P/GRAN. OPERADORES DE ENV. FRACC. P/VIA TERRESTRE'," ")
INSERT INTO f01t VALUES ('DT','IG04','IMPORTACION A CONSUMO P/GRAN. OPERADORES CON DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IG05','IMPORTACION A CONSUMO P/GRAN. OPERADORES C/DOC. TRANSP. DAP'," ")
INSERT INTO f01t VALUES ('DT','IG06','IMPORTAC. A CONSUMO P/GRAN. OPERADORES SOBRE DEP. DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IG07','IMPORTACION A CONSUMO P/GRAN. OPERADORES CON AUT. PROVISORIA'," ")
&& R.13b Mariano
INSERT INTO f01t VALUES ('DT','IG21','RETORNO EXPO TEMP C TRANSF S DOC TRANSPORTE GRAN OPERAD'," ")
INSERT INTO f01t VALUES ('DT','IG24','RETORNO EXPO TEMP C TRANSF C DOC TRANSPORTE GRAN OPERAD'," ")
INSERT INTO f01t VALUES ('DT','IG25','RETORNO EXPO TEMP C TRANSF C DOC TRANSPORTE DAP GRAN OP'," ")
INSERT INTO f01t VALUES ('DT','IG81','IMPO A CONSUMO P GRAN OPERAD DE IMPO TEMP C TRANSFOR'," ")
INSERT INTO f01t VALUES ('DT','IG82','IMPO A CONS P GRAND OPERA DE RESIDUOS DE TEMP C T'," ")
INSERT INTO f01t VALUES ('DT','IG83','IMPO A CONS P GRAN OPERADOR DE IMP TEMP S TRANSFORM'," ")
INSERT INTO f01t VALUES ('DT','IS06','IMP VEHIC REG FRANQUICIA DIPLOMATICA S DEP ALMACEN'," ")
INSERT INTO f01t VALUES ('DT','IS21','NACIONALIZ DE VEHIC INGR POR REG FRANQ DIPLOMAT'," ")
&& R.13e Mariano
INSERT INTO f01t VALUES ('DT','IT01','IMPORTACION TEMPORARIA S/TRANSFORM. S/DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IT03','IMPORT. TEMPORAL S/TRANSFORM. VIA TERREST. DE ENVIOS FRACC.'," ")
INSERT INTO f01t VALUES ('DT','IT04','IMPORTACION TEMPORARIA S/TRANSFORM. C/DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IT05','IMPORTACION TEMPORARIA S/TRANSFORM. C/DOC. TRANSP. DAP'," ")
INSERT INTO f01t VALUES ('DT','IT06','IMPORTACION TEMPORARIA S/TRANSFORM. SOBRE DEP. ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','IT07','IMPORTACION TEMPORAL REGIMEN AUTOMOTOR S/TRANSF. DAP'," ")
INSERT INTO f01t VALUES ('DT','IT11','IMPORTACION TEMPORARIA P/TRANSFORM. S/DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IT13','IMPORTACION TEMPORAL C/TRANSF. VIA TERRESTRE DE ENV. FRACC.'," ")
INSERT INTO f01t VALUES ('DT','IT14','IMPORTACION TEMPORARIA P/TRANSFORM. C/DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','IT15','IMPORTACION TEMPORARIA P/TRANSFORM. C/DOC. TRANSP. DAP'," ")
INSERT INTO f01t VALUES ('DT','IT16','IMPORTACION TEMPORARIA C/TRANSFORM. SOBRE DEP. ALMACEN.'," ")
INSERT INTO f01t VALUES ('DT','IT17','IMPORTACION TEMPORAL REGIMEN AUTOMOTOR C/ TRANSF. DAP'," ")
INSERT INTO f01t VALUES ('DT','LAPI','LICENCIA AUTOMATICA PREVIA DE IMPORTACION'," ")
INSERT INTO f01t VALUES ('DT','LMAN','LIQUIDACION MANUAL'," ")	&& R.13 Mariano
INSERT INTO f01t VALUES ('DT','LNAC','LICENCIA NO AUTOMATICA DE CALZADO'," ")
INSERT INTO f01t VALUES ('DT','LNAP','LICENCIA NO AUTOMATICA DE PAPEL'," ")
INSERT INTO f01t VALUES ('DT','RE01','REEMBARCO SIN DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','RE04','REEMBARCO CON DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','RE05','REEMBARCO CON DOCUMENTO DE TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','RE06','REEMBARCO SOBRE DESTINACION SUSP. DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','REMO','EXPORTACION POR REMOVIDO VIA ACUATICA Y TERRESTRE'," ")
INSERT INTO f01t VALUES ('DT','RTP1','RET MERC EXTRANJ AL TNC C PREV EGR TEMP A P MARINA'," ")
INSERT INTO f01t VALUES ('DT','RZ06','REZAGO DE MERC. DE DESTINACION SUSP. DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','RZF1','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF2','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF3','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF4','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF5','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF6','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF7','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','RZF8','TRANSF. E/US. DIR. Y/O INDIR. C/REENVASADO O REENVASADO USU'," ")
INSERT INTO f01t VALUES ('DT','TB04','TRASBORDO CON DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','TB05','TRASBORDO CON DOCUMENTO DE TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','TG04','IMP. TEMP. GRAN. OPER. CON DOC. DE TRANS. SIN TRANSFORMACION'," ")
INSERT INTO f01t VALUES ('DT','TG05','IMP. TEMP. GRAN. OPER. CON DOC. DE TRANS. DAP SIN TRANSF.'," ")
INSERT INTO f01t VALUES ('DT','TG06','IMP. TEMP. GRAN. OPER. SOBRE DEP. ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','TG07','IMP. TEMP. GRAN. OPER. SIN TRANSFORMACION DAP'," ")
INSERT INTO f01t VALUES ('DT','TG14','IMP. TEMP. GRAN. OPER. P/TRANSP. CON DOC. TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','TG15','IMP. TEMP. GRAN. OPER. P/TRANSP. C/DOC. DE TRANS. DAP'," ")
INSERT INTO f01t VALUES ('DT','TG16','IMP. TEMP. GRAN. OPER. C/TRANSF SOBRE DEP. ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','TG17','IMP. TEMP. GRAN. OPER. C/TRANSF. DAP'," ")
INSERT INTO f01t VALUES ('DT','TL04','TRASLADO DETALLADO CON DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','TL05','TRASLADO DETALLADO CON DOCUMENTO DE TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','TL06','TRASLADO DET. CON DOC. DE TRANSPORTE A MAS DE UN DEP. FISCAL'," ")
INSERT INTO f01t VALUES ('DT','TR01','TRANSITO DE IMPORTACION SIN DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','TR04','TRANSITO DE IMPORTACION CON DOCUMENTO DE TRANSPORTE'," ")
INSERT INTO f01t VALUES ('DT','TR05','TRANSITO DE IMPORTACION CON DOCUMENTO DE TRANSPORTE DAP'," ")
INSERT INTO f01t VALUES ('DT','TR06','TRANSITO DE IMPORTACION S/DEPOSITO DE ALMACENAMIENTO'," ")
INSERT INTO f01t VALUES ('DT','TZF1','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF2','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF3','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF4','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF5','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF6','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF7','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
INSERT INTO f01t VALUES ('DT','TZF8','TRANSFERENCIA ENTRE USUARIOS DIRECTOS Y/O INDIRECTOS'," ")
&& R.13b Mariano (estaban mal como ZFI1,ZFI2,...)
INSERT INTO f01t VALUES ('DT','ZF11','INGR. ZF DE BIENES DE CAP. LIBRE CIRC. P/RADIC. DEFIN. DE TA'," ")
INSERT INTO f01t VALUES ('DT','ZF12','INGR. ZF VITUALLAS Y MERC. DEST. CONSTRUC. INSTAL. DEL EXTER'," ")
INSERT INTO f01t VALUES ('DT','ZF13','INGR. ZF DE BIENES DE CAP. P/RADIC. DEFIN. DEL EXT'," ")
INSERT INTO f01t VALUES ('DT','ZF14','INGR. ZF MERC. LIBRE P/ALMAC./COMERC./REP. DEL T.A.'," ")
INSERT INTO f01t VALUES ('DT','ZF15','INGR. ZF MERC. ALMACENAM. Y/O COMERC. Y/O REP. DEL EXT'," ")
INSERT INTO f01t VALUES ('DT','ZF16','INGR. ZF INSUMOS LIBRE CIRC. EN FORMA TEMP. P/PROC. PROD. DESDE T.A.'," ")
INSERT INTO f01t VALUES ('DT','ZF17','INGR. ZF INSUMOS LIBRE CIRC. P/PROD. DESDE T.A.'," ")
INSERT INTO f01t VALUES ('DT','ZF18','INGR. ZF INSUMOS PARA PROCESOS PRODUCTIVOS DEL EXTERIOR'," ")
&& R.13e Mariano
INSERT INTO f01t VALUES ('DT','ZFE1','EGRESO ZF PARA CONSUMO EN EL TERRITORIO'," ")
INSERT INTO f01t VALUES ('DT','ZFE2','EGRESO POR AEROP./PUERTO DE LA ZF EN EL MISMO ESTADO AL EXT'," ")
INSERT INTO f01t VALUES ('DT','ZFE3','EGRESO ZF DE UN PROD. DE UN PROCESO PRODUCTIVO AL T.A.'," ")
INSERT INTO f01t VALUES ('DT','ZFE4','EGRESO ZF DE UN PROD. DE UN PROCESO PROD. O REPARAC. AL EXT'," ")
INSERT INTO f01t VALUES ('DT','ZFE5','EGRESO ZF RESIDUO DE UN PROCESO PRODUCTIVO C/VALOR COM AL T.A.'," ")
INSERT INTO f01t VALUES ('DT','ZFE6','EGRESO ZF RESIDUO DE UN PROCESO PRODUCTIVO C/VALOR COM AL EXT'," ")
INSERT INTO f01t VALUES ('DT','ZFER','RETORNO AL T.A. DE ACUERDO AL ART. 566 DEL C.A.'," ")
INSERT INTO f01t VALUES ('DT','ZFET','RETORNO AL T.A. DE MERCADERIA EXPORTADA TEMPORALMENTE'," ")
INSERT INTO f01t VALUES ('DT','ZFRE','REEMBARCO POR AEROPUERTO/PUERTO DE LA Z.F. MISMO ESTADO'," ")
INSERT INTO f01t VALUES ('DT','ZFTR','EG. ZF TRANSITO AL EXTERIOR EN EL MISMO ESTADO DE ING.'," ")

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC borratipos
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if used('f01t')
	sele f01t
	use
endif
dele file &d0f99.b.dbf
dele file &d0f99.b.cdx
return


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TiForm  && Ventas
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* tipo de Formularios
para t_tico

do case
	case t_tico='?'     && Ticket
		tf_retu='TZ '
	case t_tico='@'     && Recibo
		tf_retu='ABCM '
		*	case t_tico='9'     && Factura de credito
		*		tf_retu=' '
	case t_tico$'ABCM89'   && Factura NC ND RF factura de credito
		w0CoIm=iif('U'$type('f04->d4CoIm'),d0CoIm,f04->d4CoIm)
		if w0CoIm='1'
			tf_retu='ABEM '
		else
			tf_retu='CE '
		endif
	case t_tico$'6'   && ticKet-factura
		w0CoIm=iif('U'$type('f04->d4CoIm'),d0CoIm,f04->d4CoIm)
		if w0CoIm='1'
*			tf_retu='AB '	&& R.06 Mariano
*			tf_retu='ABC '	&& R.06 Mariano	&& R.13 Mariano
			tf_retu='ABCM '	&& R.13 Mariano
		else
			tf_retu=''
		endif
	&& R.13b Mariano
	case t_tico$'E.D'   && ticKet-NC/ND
		w0CoIm=iif('U'$type('f04->d4CoIm'),d0CoIm,f04->d4CoIm)
		if w0CoIm='1'
			tf_retu='ABCM '
		else
			tf_retu=''
		endif
	&& R.13e Mariano
	case t_tico='O'     && Otros
		tf_retu=' '
	&& R.17b Mariano
	case t_tico='7'      && Liquidaciones Varias
		tf_retu=' ABC'
	&& R.17e Mariano
	case t_tico$'R'   	&& Retenciones
		tf_retu=' '
	other               && Desconocido
		tf_retu=''
endcase
return tf_retu

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TiFormC  && Compras
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* tipo de Formularios
para t_tico

do case
	case t_tico$'?'      && Ticket
		tf_retu=' '
	case t_tico='@'      && Recibo
		tf_retu='ABCM '
	case t_tico$'ABCM8'   && Factura NC ND RF
		tf_retu='ABCIM '
	case t_tico='7'      && Liquidaciones Varias
*		tf_retu='ABC '	&& R.14 Mariano
		tf_retu=' ABC'	&& R.14 Mariano
	case t_tico$'6'	   && ticKet-factura
*		tf_retu='AB '	&& R.06 Mariano
*		tf_retu='ABC '	&& R.06 Mariano	&& R.13 Mariano
		tf_retu='ABCM '	&& R.13 Mariano
	&& R.13b Mariano
	case t_tico$'E.D'	   && ticKet-NC/ND
		tf_retu='ABCM '
	&& R.13e Mariano
	case t_tico$'R'   	&& Retenciones
		tf_retu='ABC '
	other                && Desconocido
		tf_retu=''
endcase
return tf_retu

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC MasUno
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wField
priv wAnte,wNume

wAnte= val(wField)
wNume= str( 10^d0ptcv+wAnte+1 , d0ptcv+1)
wNume= right( wNume,d0ptcv)
wNume= strVent(wNume)
return wNume

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC strvent
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wNume,wFila,wColu
priv wColo,wLeft,wRigh,wStri,wPosi
wPosi= at('/',wNume)
if empty(wPosi)
	wLeft= SinChar(wNume)
	wLeft= repl('0',d0ptcv)+wLeft
	wLeft= right( wLeft,d0ptcv)
	wRigh= spac(d0stcv)
else
	wLeft= left(wNume,wPosi-1)
	wLeft= SinChar(wLeft)
	wLeft= repl('0',d0ptcv)+wLeft
	wLeft= right( wLeft,d0ptcv)

	wRigh= subs(wNume,wPosi+1,12)
	if empty(wRigh)
		wRigh=spac(d0stcv)
	else
		wRigh= strtran(wRigh,' ','')
		priv i,wotro
		wOtro=.f.  && Otros caracteres que no sean numericos
		wAster=0   && Asteriscos
		for i=1 to len(wRigh)
			if ! subs(wRigh,i,1) $ '0123456789'
				wOtro=.t.
				if subs(wRigh,i,1) = '*'
					wAster=wAster+1
				endif
			endif
		next
		do case
			case wAster=3
				wRigh=repl('*',d0stcv)
			case wOtro
				wRigh= wRigh+spac(d0stcv)
				wRigh= left(wRigh,d0stcv)
			otherwise
				wRigh=right(wRigh,d0stcv)
				ValLeft=val(right(wLeft,len(wRigh)))
				do case
					case val(wRigh)>ValLeft
						&& R.16b Mariano
*						wRigh= wRigh+spac(d0stcv)
*						wRigh= left(wRigh,d0stcv)
						wRigh= left(wLeft,d0stcv-len(allt(wRigh)))+wRigh
						&& R.16e Mariano
					case val(wRigh) = ValLeft
						wRigh=spac(d0stcv)
					case val(wRigh) < ValLeft
						&& R.18b Mariano									
*						wRigh=repl('*',d0stcv)
						wRigh= strcero(str(val(left(wLeft,d0stcv-len(allt(wRigh))))+1,d0stcv-len(allt(wRigh))))+wRigh
						&& R.18e Mariano
				endcase
		endcase
	endif
endif

wStri= wLeft+'/'+wRigh
return wStri

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC SinChar
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wStri
priv i,wSin
wSin=''
for i=1 to len(wStri)
	if subs(wStri,i,1) $ '0123456789'
		wSin=wSin+subs(wStri,i,1)
	endif
next
return wSin

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function strComp
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wNume,wFila,wColu
priv wColo,wStri
wStri=alltrim(wNume)

wPosi= at('-',wStri)

if 'C'$type('wVeDa')  && Esta en actualizador, fuerza el Guiขn
	wPosi=6	&& R.24 Mariano	(6 x 5)
endif

if empty(wPosi)
	wStri=repl('0',13)+wStri
	wStri=right(wStri,13)
else
	wLeft= left(wStri,wPosi-1)
	wLeft= SinChar(wLeft)
	wLeft= repl('0',5)+wLeft	&& R.24 Mariano (5 x 4)
	wLeft= right( wLeft,5)	&& R.24 Mariano (5 x 4)
	wRigh= subs(wStri,wPosi+1,13)
	wRigh= strtran(wRigh,' ','')
	wRigh= strtran(wRigh,'-','')
	wRigh= repl('0',8)+wRigh
	wRigh= right(wRigh,8)
	wStri= wLeft+'-'+wRigh
endif
return wStri

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TcAbre
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* tipo de comprobante abreviado
para t_tico
do case
	case t_tico='?'     && Ticket
		tc_retu='TI'
	case t_tico='@'     && Recibo
		tc_retu='RE'
	case t_tico='A'     && Factura
		tc_retu='FA'
	case t_tico='B'     && Nota de debito
		tc_retu='DB'
	case t_tico='C'     && Nota de credito
		tc_retu='CR'
	case t_tico='9'     && Factura de credito
		tc_retu='FC'
	case t_tico='8'     && Recibo Factura
		tc_retu='RF'
	case t_tico='7'     && Liquidacion Varia
		tc_retu='LV'
	case t_tico='6'     && ticKet-factura
		tc_retu='TF'
	&& R.13b Mariano
	case t_tico='E'     && ticKet-NC
		tc_retu='TC'
	case t_tico='D'     && ticKet-ND
		tc_retu='TD'
	&& R.13e Mariano
	case t_tico='O'     && Otros
		tc_retu='OT'
	case t_tico='R'     && Retenciones
		tc_retu='RT'
	other               && Desconocido
		tc_retu='XX'
endcase
return tc_retu

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Readf
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Syntaxis ...: char = READF (<var1chr>,<var2chr>,<var3chr),<var4num>,[<var5num>])
*)                              alias     clave      campo    posiciones   decimales
*) Fecha.......: Junio 1988
*)
*)
*) Servicio....: Retorna el contenido de CAMPO ubicado en el archivo del ALIAS
*)               segun CLAVE, se debe informar cantidad de POSICIONES y si es
*)               numerico ademas cantidad de DECIMALES.
*)
*) Ejemplo.....: ? READF('f01',f02->d2arti,'d1desc',20)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS r_alias,r_clave,r_camp,r_posi,r_deci

if parameters()=5
	r_macro='str('+r_camp+',r_posi,r_deci)'
else
	r_macro='subs('+r_camp+',1,'+str(r_posi)+')'
endif

r_warea=SELECT(0)
SELECT (r_alias)
IF seek(r_clave,r_alias)
	r_ret=&r_macro
ELSE
	r_ret=replicate(' ',r_posi)
ENDIF
SELECT (r_warea)
RETURN(r_ret)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) Procedimientos y funciones sobre FORMULAS - IVA (windows)
*)-----------------------------------------------------------------------------
*)
*)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC ValiForm
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS wstring,wSet
wstring=upper(wstring)

*)Set de Funciones a Comparar: Clipper Memosoft
if parameters()<2
	wSet='CMP'
endif

PRIVATE wError,i,j,wAsc,wFunc,wClip,wMemo
wError=''

wClip2='AT.IF.SI'
wClip3='ABS.ASC.BOF.CHR.DAY.DOW.EOF.EXP.INT.LEN.LOG.MAX.MIN.STR.RAT.VAL.MOD.IIF'
wClip4='AINS.CDOW.CTOD.DATE.DTOC.DTOS.TRIM.TYPE.YEAR.LEFT'
wClipN='ALIAS.CMONTH.EMPTY.FOUND.LTRIM.MONTH.ROUND.SUBSTR.SPACE.UPPER.RIGHT'

wMemo='DIV.READF'

wplan='REN.SRE.TMC.IDP.REC'
*wPlan='APO.BRU.CAT.CON.DOC.EGR.FAM.FUI.ING.MAY.MEN'
*wPlan=wPlan+'.PRO.SI.TAB.TIP.POR.SAC.ESC.NET'
*wPlan=wPlan+'.TO1.TO2.TO3.TO4.TO5.TO6.READF.CAM.V1C.TIE'

for i=2 to 130
	if subs(wstring,i,1)='('
		j=i-1
		wAsc=asc(subs(wstring,j,1))
		wFunc=''
		do while wAsc>=65 .and. wAsc<=90;
				.or. wAsc>=48 .and. wAsc<=57
			wFunc=chr(wAsc)+wFunc
			j=j-1
			if j>0
				wAsc=asc(subs(wstring,j,1))
			else
				wAsc=0
			endif
		enddo
		wLen=len(wFunc)
		do case
			case empty(wFunc)
			case wLen=2  .and. wFunc $ wClip2
			case wLen=3  .and. wFunc $ wClip3
			case wLen>=4 .and. wFunc $ wClipN
			case wLen=4  .and. wFunc $ wClip4
			case wLen>=3 .and. wFunc $ wMemo .and. 'M'$wSet
			case wLen=3  .and. wFunc $ wPlan .and. 'P'$wSet
				*		case wLen>3  .and. wFunc $ 'NTOD.READF'.and. ('P'$wSet .or. 'M'$wSet)
			otherwise
				wError=iif(!empty(wError),wError+'.','')+wFunc
		endcase
	endif
next
return(wError)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC REN
*)-----------------------------------------------------------------------------
*) REN REN(RENGLON) Devuelve el Resultado OPERA ANIVEL TOTAL
*) REN
*) REN Ejemplo: REN('001',1)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS d_dato,d_colu	&& R.20 Mariano (d_colu)

do case
	case wstatus<>15 .and. parameters()>2	&& R.20 Mariano (parameters()<>1)

		*) No esta en Programa Impresor: Mal Cantidad Parametros
*		=advsuave('REN: debe llevar UN Parแmetro')	&& R.20 Mariano
		=advsuave('REN: debe llevar a lo sumo DOS Parแmetros')	&& R.20 Mariano
		werror=.t.

	case wstatus<>15 .and. type('d_dato')<>"C"

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('REN: Parแmetro debe estar entre comillas...')
		werror=.t.

	&& R.20b Mariano
	case wstatus<>15 .and. !type('d_colu')$"N.L"

		*) No esta en Programa Impresor: Parametro debe ser Numerico
		=advsuave('REN: Parแmetro debe ser Num้rico...')
		werror=.t.
	&& R.20e Mariano

	otherwise

		do case
			case wstatus=17

				if d_dato>=m.d17orde
					=advsuave('REN: '+d_dato+' debe ser menor que '+m.d17orde)
					werror=.t.
					return 0
				endif
				&& R.20b Mariano				
				if type('d_colu')="N" 
					if !betw(d_colu,1,2)
						=advsuave('REN: '+allt(str(d_colu))+' debe ser 1 o 2 ')
						werror=.t.
						return 0
					endif
				endif
				&& R.20e Mariano
				
				return 0

			case wstatus=15

				*) Esta en Emision de Informe
				&& R.20b Mariano
*				d_elem=ascan(v_indi,d_dato)

				if type('d_colu')='L'
					if !d_colu
						d_colu=1
					else
						d_colu=2
					endif
				endif
				if wdeta='S'
					d_elem=ascan(v_indi,d_dato+'zz')
					if d_elem=0
						d_elem=ascan(v_indi,d_dato)
					endif
				else
					d_elem=ascan(v_indi,d_dato)
				endif
				&& R.20e Mariano

				if !empty(d_elem)
					*if (v_calcC[d_elem]=0 .or. (v_calcC[d_elem]>0 .and. wPrPa))
					return v_calcT[d_elem,d_colu]	&& R.20 Mariano (agregue d_colu)
					*endif
				else
					return 0
				endif

		endcase

endcase
return(0)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC SRE
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*) SRE SRE(RENGLON) Devuelve el Resultado OPERA A NIVEL TOTAL
*) SRE
*) SRE Ejemplo: SRE('001','003',1)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_rd,s_rh,d_colu	&& R.20 Mariano (d_colu)

do case
	case wstatus<>15 .and. pcount()>3	&& R.20 Mariano (pcount()<>2)

		*) No esta en Programa Impresor: Mal Cantidad Parametros
*		=advsuave('SRE: debe llevar DOS Parแmetros')	&& R.20 Mariano
		=advsuave('SRE: debe llevar a los sumo TRES Parแmetros')	&& R.20 Mariano
		werror=.t.

	case wstatus<>15 .and. (type('s_rd')<>"C".or.type('s_rh')<>"C")

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('SRE: Los Parametros debe estar entre comillas...')
		werror=.t.

	&& R.20b Mariano
	case wstatus<>15 .and. !type('d_colu')$"N.L"

		*) No esta en Programa Impresor: Parametro debe ser Numerico
		=advsuave('SRE: Parแmetro debe ser Num้rico...')
		werror=.t.
	&& R.20e Mariano

	otherwise

		do case
			case wstatus=17

				if s_rd>s_rh
					=advsuave('SRE: '+s_rd+' debe ser menor que '+s_rh)
					werror=.t.
					return 0
				endif

				if s_rd>=m.d17orde
					=advsuave('SRE: '+s_rd+' debe ser menor que '+m.d17orde)
					werror=.t.
					return 0
				endif

				if s_rh>=m.d17orde
					=advsuave('SRE: '+s_rh+' debe ser menor que '+m.d17orde)
					werror=.t.
					return 0
				endif

				&& R.20b Mariano				
				if type('d_colu')="N" 
					if !betw(d_colu,1,2)
						=advsuave('SRE: '+allt(str(d_colu))+' debe ser 1 o 2 ')
						werror=.t.
						return 0
					endif
				endif
				&& R.20e Mariano

				return 0

			case wstatus=15

				*) Esta en Emision de Informe

				s_suma=0
				for s_i=val(s_rd) to val(s_rh)
					s_da=strcero(str(s_i,3))

					&& R.20b Mariano
*					d_elem=ascan(v_indi,s_da)

					if type('d_colu')='L'
						if !d_colu
							d_colu=1
						else
							d_colu=2
						endif
					endif
					if wdeta='S'
						d_elem=ascan(v_indi,s_da+'zz')
						if d_elem=0
							d_elem=ascan(v_indi,s_da)
						endif
					else
						d_elem=ascan(v_indi,s_da)
					endif
					&& R.20e Mariano

					if !empty(d_elem)
						*if (empty(v_calcC[d_elem]) .or. (v_calcC[d_elem]>0 .and. wPrPa))	
						s_suma = s_suma + v_calcT[d_elem,d_colu]	&& R.20 Mariano (agregue d_colu)
						*endif
					endif
				next
				return(s_suma)

		endcase
endcase
return(0)



*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TMC
*)-----------------------------------------------------------------------------
*) TMC Devuelve el Resultado
*) TMC
*) TMC Ejemplo: TMC('001')
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS t_ti,t_pd,t_ph

if parameters()<3
	t_pd=pedes
	t_ph=pehas
endif

do case
	case wstatus<>15 .and. !(parameters()=1.or.parameters()=3)

		*) No esta en Programa Impresor: Mal Cantidad Parametros
		=advsuave('TMC: debe llevar UNO o TRES Parametros...')
		werror=.t.

	case wstatus<>15 .and. (type('t_ti')<>"C".or.type('t_pd')<>"C".or.type('t_ph')<>"C")

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('TMC: Los Parametros deben estar entre comillas...')
		werror=.t.

	case wstatus<>15 .and. len(t_ti)<1

		*) No esta en Programa Impresor
		=advsuave('TMC: Parametro 1 debe tener 1 o mแs caract้res')
		werror=.t.

	case wstatus<>15 .and. len(t_pd)<>7

		*) No esta en Programa Impresor
		=advsuave('TMC: Parแmetro 2 debe tener 7 caracteres: MM/AAAA')
		werror=.t.

	case wstatus<>15 .and. len(t_ph)<>7

		*) No esta en Programa Impresor
		=advsuave('TMC: Parแmetro 3 debe tener 7 caracteres: MM/AAAA')
		werror=.t.

	otherwise

		t_pd=PtoI(t_pd)
		t_ph=PtoI(t_ph)

		do case
			case wstatus=17

				if t_pd>t_ph
					=advsuave('TMC: '+t_pd+' debe ser menor que '+t_ph)
					werror=.t.
					return 0
				endif
				return 0

			case wstatus=15

				*) Esta en Emision de Informe

				t_suma=0
				=start(t_pd,'f15')
				do while PtoI(f15.d15peri) <=t_ph .and. !eof('f15')

					if !f15->d15tipo$t_ti
						skip in f15
						loop
					endif

					t_suma = t_suma + f15->d15impo
					skip in f15
				enddo

				*) Enciende semaforo
				*v_calcC[wInd] = v_calcC[wInd] + 1
				return(t_suma)

		endcase

endcase
return(0)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC IDP
*)-----------------------------------------------------------------------------
*) IDP IDP(PERIODO) Incrementa, Decrementa Perกodo
*) IDP
*) IDP Ejemplo: IDP(PEDES)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS s_pe,s_va

do case
	case wstatus<>15 .and. pcount()<>2

		*) No esta en Programa Impresor: Mal Cantidad Parametros
		=advsuave('IDP: debe llevar DOS Parแmetros...')
		werror=.t.

	case wstatus<>15 .and. type('s_pe')<>"C"

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('IDP: Parแmetro 1 debe estar entre comillas...')
		werror=.t.

	case wstatus<>15 .and. len(s_pe)<>7

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('IDP: Parแmetro 1 debe tener formato MM/AAAA')
		werror=.t.

	case wstatus<>15 .and. type('s_va')<>"N"

		*) No esta en Programa Impresor: Parametro no es Numerico
		=advsuave('IDP: Parแmetro 2 debe ser num้rico...')
		werror=.t.

	otherwise

		s_me=val(left(s_pe,2))
		s_an=val(right(s_pe,4))

		do case
			case wstatus=15

				*) Esta en Emision de Informe
				t_ma = s_me + s_va    &&(Nueva cantidad de meses)
				t_an = int(t_ma/12)   &&(Aคos que involucran los nue meses)
				t_me = mod(t_ma,12)   &&(Meses con resp a 12)

				if empty(t_me)
					n_me = 12
					n_an = s_an + t_an - 1
				else
					n_me = t_me
					n_an = s_an + t_an
				endif
				return strcero(str(n_me,2))+'/'+strcero(str(n_an,4))

		endcase

endcase
return('99/9999')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC REC
*)-----------------------------------------------------------------------------
*) REC REC(RENGLON) Devuelve el Resultado OPERANDO EN EL COMPROBANTE
*) REC Ejemplo: REC('001',1)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS d_dato,d_colu	&& R.20 Mariano (d_colu)

do case
	case wstatus<>15 .and. pcount()>2	&& R.20 Mariano (pcount()<>1)

		*) No esta en Programa Impresor: Mal Cantidad Parametros
*		=advsuave('REC: debe llevar UN Parแmetro...')	&& R.20 Mariano
		=advsuave('REC: debe llevar a lo sumo DOS Parแmetros...')	&& R.20 Mariano
		werror=.t.

	case wstatus<>15 .and. type('d_dato')<>"C"

		*) No esta en Programa Impresor: Parametro no es Character
		=advsuave('REC: Parแmetro debe estar entre comillas...')
		werror=.t.

	&& R.20b Mariano
	case wstatus<>15 .and. !type('d_colu')$"N.L"

		*) No esta en Programa Impresor: Parametro debe ser Numerico
		=advsuave('REC: Parแmetro debe ser Num้rico...')
		werror=.t.
	&& R.20e Mariano

	otherwise

		do case
			case wstatus=17

				if d_dato>=m.d17orde
					=advsuave('REC: '+d_dato+' debe ser menor que '+m.d17orde)
					werror=.t.
					return 0
				endif
				&& R.20b Mariano				
				if type('d_colu')="N" 
					if !betw(d_colu,1,2)
						=advsuave('REC: '+allt(str(d_colu))+' debe ser 1 o 2 ')
						werror=.t.
						return 0
					endif
				endif
				&& R.20e Mariano

				return 0

			case wstatus=15

				*) Esta en Emision de Informe
*				d_elem=ascan(v_indi,d_dato)	&& R.20 Mariano
				d_elem=ascan(v_indi,d_dato+iif(wdeta='S','zz',''))	&& R.20 Mariano

				&& R.20b Mariano
*				d_elem=ascan(v_indi,d_dato)

				if type('d_colu')='L'
					if !d_colu
						d_colu=1
					else
						d_colu=2
					endif
				endif
				if wdeta='S'
					d_elem=ascan(v_indi,d_dato+'zz')
					if d_elem=0
						d_elem=ascan(v_indi,d_dato)
					endif
				else
					d_elem=ascan(v_indi,d_dato)
				endif
				&& R.20e Mariano

				if !empty(d_elem)
					*if (v_calcC[d_elem]=0 .or. (v_calcC[d_elem]>0 .and. wPrPa))
					return v_calcP[d_elem,d_colu]	&& R.20 Mariano (agregue d_colu)
					*endif
				else
					return 0
				endif

		endcase

endcase
return(0)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC COIM &&(Condicion Impositiva)
*)-----------------------------------------------------------------------------
*)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wCodi
priv wDesc
do case
	case wCodi='1'
		wDesc='IVA RESPONSABLE INSCRIPTO   '
	case wCodi='2'
		wDesc='IVA RESPONSABLE NO INSCRIPTO'
	case wCodi='3'
		wDesc='IVA A CONSUMIDOR FINAL      '
	case wCodi='4'
		wDesc='IVA EXENTO                  '
	case wCodi='5'
		wDesc='IVA NO RESPONSABLE          '
	case wCodi='6'
		wDesc='IVA MONOTRIBUTO             '
	case wCodi='7'
		wDesc='IVA SUJETO NO CATEGORIZADO  '
	otherwise
		wDesc='IVA CONDICION DESCONOCIDA   '
endcase
return(wDesc)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC SP
*)-----------------------------------------------------------------------------
*)
*)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS p1
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC MEMOLINE
*)-----------------------------------------------------------------------------
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS pmemo,ptam,pline

PRIVATE w_oldmwidth,w_retu
w_oldmwidth=SET('MEMOWIDTH')

set memowidth to ptam
w_retu=MLINE(pmemo,pline)
set memowidth to (w_oldmwidth)

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func TTiOp
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para t_tipo,t_tico,t_tifo
priv t_tiop

do case

	case t_tifo$'C. '
		t_tiop='E'            && Exenta

	case t_tifo$'I'
*		t_tiop='I'            && Importacion	&& R.13 Mariano
		t_tiop='X'            && Importacion	&& R.13 Mariano

	case t_tifo$'E'
*		t_tiop='Y'            && Exportacion	&& R.13 Mariano
		t_tiop='X'            && Exportacion	&& R.13 Mariano

	other
*		t_tiop='O'            && Gravada	&& R.13 Mariano
		t_tiop='0'            && Gravada	&& R.13 Mariano

endcase
return t_tiop

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TCoCo &&(Codigo de Comprobante)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*para t_tico,t_tifo					&& R.07 Mariano
para t_tico,t_tifo, t_tipo			&& R.07 Mariano
priv t_coco

t_tico=TcAbre(t_tico)

*t_coco='99'	&& R.14 Mariano
t_coco='099'	&& R.14 Mariano

if empty(t_tipo)	&& R.07 Mariano
	t_tipo=''		&& R.07	Mariano
endif				&& R.07 Mariano

do case
	case t_tico$'FA'     && Factura
		do case
			case t_tifo='A'
				t_coco='001'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='006'	&& R.13 Mariano
			case t_tifo='C'
				t_coco='011'	&& R.13 Mariano
			case t_tifo='M'
				t_coco='051'	&& R.13 Mariano
			case t_tifo	= 'E'     && Factura Exportaci๓n
				t_coco='019'	&& R.13 Mariano
			case t_tifo	= 'I'     && Documento Aduanero
*				t_coco='14'		&& R.13 Mariano
				t_coco='066'		&& R.13 Mariano
		endcase

	case t_tico$'FC'     && Factura Cr้dito????????
		do case
			case t_tifo='A'
				t_coco='001'	&& R.13 Mariano
		endcase

	case t_tico$'RF'     && Recibo Factura????????????
		do case
			case t_tifo='A'
				t_coco='004'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='009'	&& R.13 Mariano
			case t_tifo='C'
				t_coco='015'	&& R.13 Mariano
			&& R.13b Mariano
			case t_tifo='M'
				t_coco='054'
			&& R.13e Mariano
		endcase

	case t_tico$'CR'     && Notas de Cr้dito
		do case
*			case t_tipo='C'		&& R.07 Mariano						&& R.14 Mariano
*				t_coco='042'		&& R.07 Mariano	&& R.13 Mariano	&& R.14 Mariano
			case t_tifo='A'
				t_coco='003'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='008'	&& R.13 Mariano
			case t_tifo='C'
				t_coco='013'	&& R.13 Mariano
			case t_tifo='M'
				t_coco='053'	&& R.13 Mariano
			case t_tifo	= 'E'     && Factura Exportaci๓n
*				t_coco='099'	&& R.13 Mariano	&& R.14 Mariano
				t_coco='090'	&& R.14 Mariano
			case t_tifo	= 'I'     && Documento Aduanero
*				t_coco='099'	&& R.13 Mariano	&& R.14 Mariano
				t_coco='090'	&& R.14 Mariano
		endcase

	case t_tico$'DB'     && Notas de D้bito
		do case
			case t_tifo='A'
				t_coco='002'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='007'	&& R.13 Mariano
			case t_tifo='C'
				t_coco='012'	&& R.13 Mariano
			case t_tifo='M'
				t_coco='052'	&& R.13 Mariano
			case t_tifo	= 'E'     && Factura Exportaci๓n
				t_coco='099'	&& R.13 Mariano
			case t_tifo	= 'I'     && Documento Aduanero
				t_coco='099'	&& R.13 Mariano

		endcase

	case t_tico$'TI'  AND t_tifo <> "Z"    && Ticket com๚n
		t_coco='083'	&& R.13 Mariano
		
	case t_tico$'TI'  AND t_tifo = "Z"    && Ticket Z
*		t_coco='080'	&& R.13 Mariano	&& R.24 Mariano
		t_coco='083'	&& R.13 Mariano	&& R.24 Mariano

	case t_tico$'TF'     && Ticket-Factura
		do case
			case t_tifo='A'
				t_coco='081'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='082'	&& R.13 Mariano
			&& R.13b Mariano
			case t_tifo='C'
				t_coco='111'
			case t_tifo='M'
				t_coco='118'
			&& R.13e Mariano
		endcase
	&& R.13b Mariano
	case t_tico$'TC'     && Ticket-NC
		do case
			case t_tifo=' '
				t_coco='110'
			case t_tifo='A'
				t_coco='112'
			case t_tifo='B'
				t_coco='113'
			case t_tifo='C'
				t_coco='114'
			case t_tifo='M'
				t_coco='119'
		endcase
	case t_tico$'TD'     && Ticket-ND
		do case
			case t_tifo='A'
				t_coco='115'
			case t_tifo='B'
				t_coco='116'
			case t_tifo='C'
				t_coco='117'
			case t_tifo='M'
				t_coco='120'
		endcase
	&& R.13e Mariano
	case t_tico$'RE'     && Recibo
		do case
			case t_tifo='A'
				t_coco='004'	&& R.13 Mariano
			case t_tifo='B'
				t_coco='009'	&& R.13 Mariano
			case t_tifo='C'
				t_coco='015'	&& R.13 Mariano
			case t_tifo='M'
				t_coco='054'	&& R.13 Mariano
		endcase

	case t_tico$'LV'     					&& Liquidaciones Varias
		do case
			case t_tifo='A'
						t_coco='063'	&& R.13 Mariano
			case t_tifo='B'
						t_coco='064'	&& R.13 Mariano
			case t_tifo='C'
*						t_coco='65'		&& R.13 Mariano
						t_coco='068'	&& R.13 Mariano
			case t_tifo='M'				&& R.13 Mariano
						t_coco='059'	&& R.13 Mariano
		endcase

endcase
SHOW GET t_coco
return(t_coco)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC teranor
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if sino('Confirma ?', 'NO', 'Terminaciones Anormales')='S'

	if !lockemp(nf_coem)

		=errornet(050)

	else

		do db_indexiw with .t.

		=unlockemp(nf_coem)

		=closall()

	endif

endif

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC DispVent
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wNume,wcompr
priv wPosi
wPosi= at('/',wNume)
if wPosi>0
	if empty(right(wNume,d0stcv))
		wNume=strtran(wNume,'/',' ')
	else
		&& R.16b Mariano
		if !empty(wcompr)
			do case
				case len(allt(wNume))=13
*					wNume=wNume	&& R.17 Mariano
					wNume=rtrim(wNume)	&& R.17 Mariano
				case left(wNume,4)=subst(wNume,wPosi+1,4)
					wNume=left(wNume,wPosi)+right(wNume,d0stcv-4)
				case empty(subst(wNume,wPosi+1,4))	&& R.17 Mariano
					wNume=rtrim(wNume)	&& R.17 Mariano
				otherwise
					wNume=left(wNume,wPosi)+space(4)+wcompr+' '+right(wNume,d0stcv)
			endcase
		endif
		&& R.16e Mariano
	endif
endif
return wNume

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC SetComp
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
publ wFiltCOM,wComDB,wComCR,wSignDB,wSignCR

wComDB='002.007.012.'	&& R.13 Mariano (agregue 0)
wComCR='003.008.013.'	&& R.13 Mariano (agregue 0)
wFiltCOM=''

do case
	case f12->p0tonc='N'
		wFiltCOM=wFiltCOM+wComCR
		wSignCR=0
	case f12->p0tonc='S'
		wSignCR=1
	case f12->p0tonc='R'
		wSignCR=-1
endcase
do case
	case f12->p0tond='N'
		wFiltCOM=wFiltCOM+wComDB
		wSignDB=0
	case f12->p0tond='S'
		wSignDB=1
	case f12->p0tond='R'
		wSignDB=-1
endcase
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func CreaMov
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para c_desde,c_hasta,c_tipo,c_clas,c_todo,c_acum
priv c_retu
c_retu=.t.


if type('c_todo')$'U'
	c_todo=.f.
endif

c_desde=PtoI(c_desde)
c_hasta=PtoI(c_hasta)

wait wind 'Creando Archivo Transitorio...' nowait

sele f05
copy stru exte to &d0f99.s
wfi9s=d0f99+'s'
if !used('f9s')
	use (wfi9s) in 0 alias 'f9s'
endif
sele f9s
appe blan
repl field_name with 'd5cuit', field_type with 'C', field_len with 13
appe blan
repl field_name with 'd5clas', field_type with 'C', field_len with  1
appe blan
repl field_name with 'd5nocl', field_type with 'C', field_len with 35
appe blan
repl field_name with 'd5tido', field_type with 'C', field_len with 2
appe blan
repl field_name with 'd5neto', field_type with 'N', field_len with 19 , field_dec with 2
appe blan
repl field_name with 'd5sele', field_type with 'C', field_len with 1
appe blan
repl field_name with 'd5marc', field_type with 'C', field_len with 1

use

create &d0f99.c from &d0f99.s
use
dele file &d0f99.s.dbf

wfi9c=d0f99+'c'
if !used('f9c')
	use (wfi9c) in 0 alias 'f9c' exclu
endif
sele f9c

wait clear
wRec=0

sele f05
=start(c_desde,'f05')
do while PtoI(f05->d5peri) <= c_hasta and !eof('f05')

	if inkey()=p_Esc
		s_sino=2
		if sino('Interrumpe ?.','N')='S'
			set curs off
			c_retu=.f.
			exit
		endif
		set curs off
	endif

	if !f05->d5tipo $ c_tipo
		skip in f05
		loop
	endif

	wInCi='S'
	wCuit=TomCui(@wInCi)

	if (!empty(wCuit).or.c_todo) .and. !wInCi='N'

		wRec = wRec + 1

		for _i=1 to fcount()
			wFie=field(_i)
			wWor='w'+subs(wFie,2,6)
			if type('&wFie')$'N'
				* Se Inicializan en cero para que no sumen doble
				&wWor = 0
			else
				&wWor = f05->&wFie
			endif
		next

		wClave=w5peri+w5tipo+dtos(w5fech)+w5sucu+w5tico+w5tifo+w5nume+w5clie

		if !c_acum
			
			do while wClave=f05.d5peri+f05.d5tipo+dtos(f05.d5fech)+f05.d5sucu+f05.d5tico+f05.d5tifo+f05.d5nume+f05.d5clie

				*)Mientras se Mantenga el Mismo Comprobante

				for _i=1 to fcount()
					wFie=field(_i)
					wWor='w'+subs(wFie,2,6)
					if type('&wWor')$'N'
						* Se Acumulan los Importes Numericos
						* Tomando el Valor absoluto para las Notas de Credito
						&wWor = &wWor + iif(f05.d5tota<0,(-1),1)*(f05->&wFie)	&& R.14 Mariano (reemplace abs() por iif(w5tota<0,(-1),1)* )
					endif
				next

				skip in f05

			enddo
		
		skip -1 in f05

		else
				for _i=1 to fcount()
					wFie=field(_i)
					wWor='w'+subs(wFie,2,6)
					if type('&wWor')$'N'
						* Se Acumulan los Importes Numericos
						* Tomando el Valor absoluto para las Notas de Credito
						&wWor = iif(w5tota<0,(-1),1)*(f05->&wFie)	&& R.14 Mariano (reemplace abs() por iif(w5tota<0,(-1),1)* )
					endif
				next

		endif
		=net_appe('f9c')

		*) Graba datos de W5 a f9c->d5
		for _i=1 to fcount()
			wFie=field(_i)
			wWor='w'+subs(wFie,2,6)
			repl f9c->&wFie with &wWor
		next

		wClas=readf('f01','R'+d5tipo+d5rubr,'f01->d1clas',1)

		do case
			case wClas$'A.B.C.D.E'
				wClas='1'
			case wClas$'F'
				wClas='2'
			case wClas$'G'
				wClas='3'
		endcase

		repl f9c->d5nocl with TomNom()                   ,;
			f9c->d5cuit with wCuit                      ,;
			f9c->d5tido with TomTiD()                   ,;
			f9c->d5clas with iif(wClas$c_clas,'1',wClas) ,;
			f9c->d5neto with TomNeto()

		skip in f05
	else
		skip in f05
	endif

enddo

sele f9c
inde on d5tipo+d5cuit+PtoI(d5Peri)+d5tiop+d5coco+d5tifo+d5nume tag d5tipo
go top
return(c_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func Sele860
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para s_CoTo   && Controla los topes ?
priv w_alguno
w_alguno=.f.

wait wind 'Analizando Clientes-Proveedores...' nowait

if s_CoTo='S'
	wRec=0
	go top
	do while !eof()

		stor 0 to wBien,wServ
		wEntraB=.f.
		wEntraS=.f.

		w_rec=recno()

		Stor 0 to VMaxOpB,VMaxOpS

		MaxTrB =f12->p0iTrB   && Tope del Trimestre Bienes
		MaxTrS =f12->p0iTrS   && Tope del Trimestre Servicios
		MaxTrBS=9999999999    && Tope de Ambos

		MaxOpb =f12->p0iOpB
		MaxOpS =f12->p0iOpS

		w5cuit=d5cuit
		do while w5cuit=d5cuit

			wRec = wRec + 1
			w5tota=d5tota*iif(d5coco$wComDB,wSignDB,iif(d5coco$wComCR,wSignCR,1))

			do case
				case d5coco $ wFiltCOM
					* Excluye DE CR

				case d5clas$'12'
					wBien = wBien + w5tota
					if d5tota>VMaxOpB
						VMaxOpB=d5tota
						if VMaxOpB>=f12->p0iopb
							wEntraB=.t.
							exit
						endif
					endif

				case d5clas$'3'
					wServ  = wServ + w5tota
					if d5tota>VMaxOpS
						VMaxOpS=d5tota
						if VMaxOpS>=f12->p0iops
							wEntraS=.t.
							exit
						endif
					endif

			endcase


			skip
		enddo

		if !empty(wServ) .and. !empty(wBien)
			* Hubo alguna Operaciขn de Anexo 1
			MaxTrB =f12->p0xTrX
			MaxTrS =f12->p0xTrX

			MaxTrBS=f12->p0xTrX        && Ambos

			MaxOpb =f12->p0xOpB
			MaxOpS =f12->p0xOpS

			if VMaxOpB>=MaxOpB
				wEntraB=.t.
			endif
			if VMaxOpS>=MaxOpS
				wEntraS=.t.
			endif

		endif

		if wEntraB .or. wEntraS .or. wBien>=MaxTrB .or. wServ>=MaxTrS;
				.or. wBien+wServ>=MaxTrBS

			w_alguno = .t.

			go w_rec
			do while w5cuit=d5cuit
				wMarca=' '
				if wEntraB
					if d5tota>=MaxOpB .and. d5clas$'12'
						wMarca='#'
					endif
				endif
				if wEntraS
					if d5tota>=MaxOpS .and. d5clas$'3'
						wMarca='#'
					endif
				endif
				repl d5sele with '*'    ,;
					d5marc with wMarca
				skip
			enddo
		endif

	enddo
else
	* No Controla, Marca Todo, se Usa para Bajada completa de Registros
	repl all d5sele with '*'
	w_alguno = .t.
endif

wait clear
Return(w_alguno)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TomNom
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv wNomb
wNomb=spac(35)

if d5clie<'99990'
	if d5tipo='V'
		wNomb=readf('f02',d5clie,'f02->d2nomb',35)
	else
		wNomb=readf('f03',d5clie,'f03->d3nomb',35)
	endif
else
	wNomb=readf('f06',d5inte,'f06->d6nocl',35)
endif
return upper(wNomb)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TomCui
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para t_inci

priv wCuit
wCuit=spac(13)

if d5clie<'99990'
	if d5tipo='V'
		wCuit=readf('f02',d5clie,'f02->d2cuit',13)
		t_inci=f02->d2inci
	else
		wCuit=readf('f03',d5clie,'f03->d3cuit',13)
		t_inci=f03->d3inci
	endif
else
	wCuit=readf('f06',d5inte,'f06->d6cuit',13)
	if d5tipo='V'
		t_inci=readf('f02',d5clie,'f02->d2inci',1)
	else
		t_inci=readf('f03',d5clie,'f03->d3inci',1)
	endif
endif
return upper(wCuit)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TomTiD
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
wTido=spac(02)

if d5clie<'99990'
	if d5tipo='V'
		wTido=readf('f02',d5clie,'f02->d2tido',02)
	else
		wTido=readf('f03',d5clie,'f03->d3tido',02)
	endif
else
	wTido=readf('f06',d5inte,'f06->d6Tido',02)
endif
return upper(wTido)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC TomNeto
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv t_retu
t_retu=0
if d5tiop$'O'
	t_retu=w5imp1
endif
if empty(t_retu)
	t_retu=w5tota
endif
return(iif(w5tota<0,(-1),1)*(t_retu))	&& R.14 Mariano (reemplace abs() por iif(w5tota<0,(-1),1)* )

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC MaT
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para m_peri
m_mes=val(left(m_peri,2))

r_tri=int((m_mes-1)/3)+1
r_mes=iif(empty(mod(m_mes,3)),3,mod(m_mes,3))
r_ano=right(m_peri,2)
return 'TR:'+str(r_tri,1)+' Aฅ:'+r_ano+' ME:'+str(r_mes,1)


*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function VerCAI		&& verifica en CAI del comprobante
*-F อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_nume

if parameter()=0
	w_nume=''
endif

private w_i, w_retu, w_et1, w_et2, w_et3, w_et4, w_et5
w_retu=''

if !empty(w_nume)
	w_et1=0
	for w_i=1 to 13 step 2
		w_et1=w_et1+val(substr(w_nume,w_i,1))
	endfor
	w_et2=w_et1*3
	w_et3=0
	for w_i=2 to 14 step 2
		w_et3=w_et3+val(substr(w_nume,w_i,1))
	endfor
	w_et4=w_et2+w_et3
	w_et5=mod(w_et4,10)
	w_retu=str(w_et5,1)
endif

return(w_retu)







*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
function audi
*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para a_tiop,a_leyd,a_leyh,a_coem

if len(a_tiop)>3
	* Si tiop tiene + de 3 fuerza la grabaciขn
	a_audi='S'
	a_tiop=left(a_tiop,3)
else
	a_audi=d0audi
endif

if a_audi<>'S'
	return ''
endif

do case
	case parameters()=1
		a_leyd=''
		a_leyh=''
		a_coem=d0coem

	case parameters()=2
		a_leyh=''
		a_coem=d0coem

	case parameters()=3
		a_coem=d0coem

endcase

a_abrio=.f.
if !used('f22')
	a_alias=alias()

	use &d0f22 in 0 alias f22 order d22empr		&& Auditoria

	a_abrio=.t.
endif

sele f22

=net_appe('f22')

repl d22empr with a_coem,;
	d22fech with date(),;
	d22time with time(),;
	d22tiop with a_tiop

repl d22lede with a_leyd,;
	d22leha with a_leyh


if a_abrio
	use

	if !empty(a_alias) and used(a_alias)
		sele &a_alias
	endif
endif

return ''

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC f_set
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para f_pan
priv memo_lin,in,i,j,lstri,long,ro,co,wsen,palabra,l

if !localiza('ffo','d55Codi=f_pan')
	f_pan='A'+right(f_pan,1)	&& impresion por defecto
	if !localiza('ffo','d55Codi=f_pan')
		return .f.
	endif
endif

w_oldmemo=set('MEMOWIDTH')
set memowidth to 161

*) Estas variables seran usadas por las funciones de impresion
public v_det_ini  && elemento de inicio de detalle en vector
public v_det_fin  && elemento de fin    de detalle en vector
public v_pie_fin  && elemento de fin    de pie     en vector
public v_reg_ini  && elemento de inicio del registro en base transitoria
v_det_ini=0
v_det_fin=0
v_pie_fin=0
v_reg_ini=0

*) Estas variables pueden usarse externamente
public f_pie_ini  && fila de inicio de pie en formulario
public f_cal_for  && cantidad de lineas del   formulario
public f_cal_det  && cantidad de lineas del detalle del formulario
store 0 to f_pie_ini,f_cal_for,f_cal_det

public f_vec[100] && vector con expresion a evaluar
public f_row[100] && vector con fila
public f_col[100] && vector con columna

public f_cacp

f_vec=''
f_row=''
f_col=''

memo_lin=memlines(ffo->d55memo)

f_cacp=1
if !'U'$type('ffo->d55cacp')
	f_cacp=ffo->d55cacp
endif
if empty(f_cacp)
	f_cacp=1
endif

in=0
v_det_ini=0
v_det_fin=0
m_ini_pie=0

for mLine=2 to memo_lin &&(Arranca en 2 para saltear regleta)

	lstri=trim(mline(ffo->d55memo,mLine))
	long =len(lstri)

	ro=mLine-2-m_ini_pie
	co=0

	wsen = .t.
	palabra = ''

	for j=1 to long

		if subs(lstri,1,2)='@I'
			* Esta marca indica que empieza el detalle, seguidamente
			* encontramos la cantidad de palabras del detalle

			v_det_ini = in + 1  && elemento del vector con el que empieza la
			&& zona de detalle

			f_cal_det = val(subs(lstri,3,3))

			f_pie_ini = ro + f_cal_det   && variable externa que indica
			&& la fila en la que comienza el pie
			exit
		else
			if subs(lstri,1,2)='@F'
				* Esta marca indica el fin de la zona de detalle

				v_det_fin = in  && elemento del vector donde finaliza el detalle

				m_ini_pie = ro  && esta variable hace que las filas del pie
				&& empiecen en cero, todo sera relativo a f_pie_ini
				&& ver funcion f_pie

				f_cal_for = val(subs(lstri,3,3))   && variable externa que
				&& indica la cantidad de
				&& lineas del formulario
				exit
			endif
		endif

		l=subs(lstri,j,1) && toma caracter

		do case
			case l==' '
				* Si es blanco y no se trata del fin de una palabra
				* sigue de largo

				if ! empty (palabra)
					* Si entra por aca indica que es el fin de una palabra
					* Si se estaba armando una palabra la cierra

					palabra = palabra + '"'
					in = in + 1

					* Carga de los vectores
					f_row[in] = ro
					f_col[in] = co
					f_vec[in] = palabra

					palabra = ''

					wsen = .t.  && Esta senal forzara la inicializacion de un
					&& nuevo string cuando pase por el ultimo case
				endif

			case l=='>'
				* Aqui es cuando encuentra una variable

				if ! empty (palabra)
					* Si se estaba armando una palabra la cierra

					palabra = palabra + '"'
					in = in + 1

					* Carga de los vectores
					f_row[in] = ro
					f_col[in] = co
					f_vec[in] = palabra

					palabra = ''
				endif

				* Saltea el ">"
				co = j - 1 && la resta es para que exista row 0
				j=j+1
				l=upper(subs(lstri,j,1))

				in = in + 1

				* Carga de los vectores
				f_row[in] = ro
				f_col[in] = co

				if seek(l,'fex')
					* Si existe la variable carga la expresion
					f_vec[in] = alltrim(fex->d44expr)
				else
					* Si no existe arma mensaje de error
					f_vec[in] = ["&l" +' ERROR']
				endif

				wsen = .t.  && Esta senal forzara la inicializacion de un
				&& nuevo string cuando pase por el ultimo case

			case ! l==' '

				if wsen
					* Inicia armado de una nueva palabra
					co = j - 1  && la resta es para que exista row 0
					wsen = .f.
					palabra = '"'
				endif

				palabra = palabra + l && aqui se va armando la palabra
		endcase
	next
	if ! empty(palabra)
		* Si se estaba armando una palabra la cierra
		palabra = palabra + '"'
		in = in + 1

		f_row[in] = ro
		f_col[in] = co
		f_vec[in] = palabra

		palabra = ''
	endif
next

v_pie_fin = in   && ultimo elemento del vector

set memowidth to (w_oldmemo)
return(.t.)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC f_cabe
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv cLine,aLine,macro,wreng
cLine=1
wreng=''
aLine=f_row[cLine]

do while cLine <= v_det_ini-1 .and. p_cancela
	macro=f_vec[cLine]
	wtype=mtype(macro)
	*	if wexist('iws980')
	*		wType=type("&macro")
	*	else
	*		wType=&Macro
	*		wtype=type('wtype')
	*	endif
	if wType=='U';
			.or. wType=='UE';
			.or. wType=='UI' .and. ! 'U'$type('wPrueba')
		if f_row[cLine]=aLine
			if "sp('comp')"$f_vec[cLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[cLine])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[cLine])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+wtype
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[cLine-1]-1
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,'N',w_grupo)
			if "sp('comp')"$f_vec[cLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[cLine])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[cLine])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+wtype
			aLine=f_row[cLine]
		endif
	else
		if f_row[cLine]=aLine
			if "sp('comp')"$f_vec[cLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[cLine])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[cLine])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+&macro
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[cLine-1]-1
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,iif("sp('comp')"$f_vec[cLine-1],'C','N'),w_grupo)
			if "sp('comp')"$f_vec[cLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[cLine])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[cLine])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+&macro
			aLine=f_row[cLine]
		endif
	endif
	cLine=cLine+1
enddo
if f_row[cLine-1]=aLine
	sele f99c
	go bott
	for j=recno()-v_reg_ini to f_row[cLine-1]-1
		insert into (alias()) values ('','N',w_grupo)
	endfor
	insert into f99c values (wreng,iif("sp('comp')"$f_vec[cLine-1],'C','N'),w_grupo)
endif
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC f_deta
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para f_lin
priv dVari,macro

wreng=''
dVari=v_det_ini
advari=f_row[dvari]+f_lin-1

do while dVari <= v_det_fin .and. p_cancela
	macro=f_vec[dVari]
	wtype=mtype(macro)
	*	if wexist('iws980')
	*		wType=type("&macro")
	*	else
	*		wType=&Macro
	*		wtype=type('wtype')
	*	endif
	if wType=='U';
			.or. wType=='UE';
			.or. wType=='UI' .and. ! 'U'$type('wPrueba')
		*)@ f_row[dVari]+f_lin-1,f_col[dVari] say wType

		if f_row[dvari]+f_lin-1=advari
			if "sp('comp')"$f_vec[dVari]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[dVari])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[dVari])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+wtype
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[dvari-1]+f_lin-2
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,'N',w_grupo)
			if "sp('comp')"$f_vec[dVari]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[dVari])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[dVari])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+wtype
			advari=f_row[dvari]+f_lin-1
		endif

	else
		*)@ f_row[dVari]+f_lin-1,f_col[dVari] say &macro

		if f_row[dvari]+f_lin-1=advari
			if "sp('comp')"$f_vec[dVari]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[dVari])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[dVari])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+&macro
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[dvari-1]+f_lin-2
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,iif("sp('comp')"$f_vec[dvari-1],'C','N'),w_grupo)
			if "sp('comp')"$f_vec[dVari]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[dVari])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[dVari])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+&macro
			advari=f_row[dvari]+f_lin-1
		endif

	endif
	dVari=dVari+1
enddo
if f_row[dvari-1]+f_lin-1=advari
	sele f99c
	go bott
	for j=recno()-v_reg_ini to f_row[dvari-1]+f_lin-2
		insert into (alias()) values ('','N',w_grupo)
	endfor
	insert into f99c values (wreng,iif("sp('comp')"$f_vec[dvari-1],'C','N'),w_grupo)
endif
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC f_pie
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv pLine,macro

pLine=v_det_fin+1
wreng=''
apline=f_row[pLine]+f_pie_ini

do while pLine <= v_pie_fin .and. p_cancela
	macro=f_vec[pLine]
	wtype=mtype(macro)
	*	if wexist('iws980')
	*		wType=type("&macro")
	*	else
	*		wType=&Macro
	*		wtype=type('wtype')
	*	endif
	if wType=='U';
			.or. wType=='UE';
			.or. wType=='UI' .and. ! 'U'$type('wPrueba')
		*)@ f_pie_ini+f_row[pLine],f_col[pLine] say wType

		if f_row[pline]+f_pie_ini=apline
			if "sp('comp')"$f_vec[pLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[pLine])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[pLine])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+wtype
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[pline-1]+f_pie_ini-1
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,'N',w_grupo)
			if "sp('comp')"$f_vec[pLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[pLine])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[pLine])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+wtype
			apline=f_row[pline]+f_pie_ini
		endif

	else

		*)@ f_pie_ini+f_row[pLine],f_col[pLine] say &macro

		if f_row[pline]+f_pie_ini=apline
			if "sp('comp')"$f_vec[pLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[pLine])/10*18)-(iif(len(wreng)=0,0,len(wreng))),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[pLine])/10*14)-(iif(len(wreng)=0,0,len(wreng))),0)
			endif
			wreng=wreng+space(iif(w_spc<=0, 0, w_spc))+&macro
		else
			sele f99c
			go bott
			for j=recno()-v_reg_ini to f_row[pline-1]+f_pie_ini-1
				insert into (alias()) values ('','N',w_grupo)
			endfor
			insert into f99c values (wreng,iif("sp('comp')"$f_vec[pline-1],'C','N'),w_grupo)
			if "sp('comp')"$f_vec[pLine]
				*transforma la columna para 17 c/"
				w_spc=round(((f_col[pLine])/10*18),0)
			else
				*transforma la columna para 13 c/"
				w_spc=round(((f_col[pLine])/10*14),0)
			endif
			wreng=space(iif(w_spc<=0, 0, w_spc))+&macro
			apline=f_row[pline]+f_pie_ini
		endif

	endif
	pLine=pLine+1
enddo
if f_row[pline-1]+f_pie_ini=apline
	sele f99c
	go bott
	for j=recno()-v_reg_ini to f_row[pline-1]+f_pie_ini-1
		insert into (alias()) values ('','N',w_grupo)
	endfor
	insert into f99c values (wreng,iif("sp('comp')"$f_vec[pline-1],'C','N'),w_grupo)
endif
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC f_rel
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
rele v_det_ini  && elemento de inicio de detalle en vector
rele v_det_fin  && elemento de fin    de detalle en vector
rele v_pie_fin  && elemento de fin    de pie     en vector
rele v_reg_ini  && elemento de inicio del registro en base transitoria

*) Estas variables pueden usarse externamente
rele f_pie_ini  && fila de inicio de pie en formulario
rele f_cal_for  && cantidad de lineas del   formulario
rele f_cal_det  && cantidad de lineas del detalle del formulario

rele f_vec      && vector con expresion a evaluar
rele f_row      && vector con fila
rele f_col      && vector con columna
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC Localiza &&(Localiza(Alias,Condicion))
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS wAlia,wCond
PRIVATE wAlAn,wExis
wAlAn=''
wExis=.f.
if wAlia <> alias()
	wAlAn=alias()
	sele &wAlia
endif
go top
loca for &wCond
if ! eof()
	wExis=.t.
endif
if !empty(wAlAn)
	sele &wAlAn
endif
return(wExis)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC deletran
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private all like w_*
declare w_temp[1]
wwcant=adir(w_temp,'\iw\transfer.00?')
wfinu=0
for wfinu=1 to wwcant
	wmadel='\iw\'+w_temp[wfinu,1]
	dele file &wmadel
next

return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func ftiimp
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* Retorna Categoria Impositiva segun cliente-proveedor y periodo
para f_clpr,f_peri,f_tipo
priv f_ret,f_rec

&& R.22b Mariano
private f_ord02,f_ord03

f_ord02=order('f02')
f_ord03=order('f03')
&& R.22e Mariano

f_peri=ptoi(f_peri)

f_ret='X'

if seek(f_tipo+f_clpr+f_peri,'f20')
	f_ret=f20->d20tiim
else
	=start(f_tipo+f_clpr+f_peri,'f20')
	skip -1 in f20

	if f20->d20clie=f_clpr .and. f20->d20tipo=f_tipo
		f_ret=f20->d20tiim
	else
		if f_tipo='C'
			if !eof('f02')
				f_rec=recno('f02')
			else
				f_rec=recno('f02')-1
			endif
			set order to d2clie in f02	&& R.22 Mariano
			if seek(f_clpr,'f02')
				f_ret=f02->d2tipo
			endif
			set order to &f_ord02 in f02	&& R.22 Mariano
			go f_rec in f02
		else
			if !eof('f03')
				f_rec=recno('f03')
			else
				f_rec=recno('f03')-1
			endif
			set order to d3prov in f03	&& R.22 Mariano
			if seek(f_clpr,'f03')
				f_ret=f03->d3tipo
			endif
			set order to &f_ord03 in f03	&& R.22 Mariano
			go f_rec in f03
		endif
	endif
endif

return f_ret

*=====================================================================================
PROCEDURE reten
*=====================================================================================

pc_pr	= ""							&&		f00.pr {c/v} {1/2/3}
pc_imp	= ""							&&		d4 {c/v} im {6/7/8}
pc_tota	= ""							&&		d4ctot o d4vtot

pn_n	= 4								&&		= 4/6/7/8		4 = SIN ASIGNAR
SELE f04

w_retu	= .F.

*f00.p0pr{c/v}[n]							n = 1, 2, 3
*					= 1 va en {c/v}im6
*					= 2 va en {c/v}im7
*					= 3 va en {c/v}im8					

IF m.d5tipo	= "V"	OR	m.d5tipo	= "Y"
			pc_imp	= "d4vim"
			pc_pr	= "f00.p0prv"
ELSE
	pc_imp	= "d4cim"
	pc_pr	= "f00.p0prc"
ENDIF

DO CASE
		CASE !EMPTY(m.d5pere)		&& IVA 
						pn_n = 3
		CASE !EMPTY(m.d5cpib)		&& Ingresos Brutos
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 1
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
		CASE !EMPTY(m.d5cpga)		&& Ganancias
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 2
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
		CASE !EMPTY(m.d5cpot)		&& Otros 
						FOR pn_n = 1 TO 3
								pc_pr	= SUBSTR(pc_pr,1,9) + STR(pn_n,1)
								IF &pc_pr = 3
										pn_n = pn_n + 4
								ENDIF
						ENDFOR
ENDCASE

IF pn_n <> 4
		pc_imp	= pc_imp + STR(pn_n,1)

		REPLACE &pc_imp WITH &pc_imp + m.d5tota

		w_retu	= .T.
ENDIF

IF w_retu
	IF m.d5tipo	= "V"	OR	m.d5tipo	= "Y"
				REPLACE d4vtot WITH d4vtot + m.d5tota
				pc_tota = "d4vtot"
	ELSE
		REPLACE d4ctot WITH d4ctot + m.d5tota
		pc_tota = "d4ctot"
	ENDIF
ENDIF

RETURN

*================================================================================
PROCEDURE listalib
*=====================================================================================

PARAMETERS w_oper, w_rep

* Ver IVA, SICOL, etc

IF parameters()	= 0
			IF !w_print
					DO REPDEST							&&(Setea variables internas del reporte)
					w_print = .T.
			ENDIF
ELSE
	IF LEFT(w_oper,1)	= "O"	OR 	LEFT(w_oper,1)	= "C"
						w_name	= LEFT(w_name,2) + RIGHT(DTOC(whfech),2) + SUBS(DTOC(whfech),4,2)
	ENDIF

	IF !w_print
			DO REPDEST1 WITH w_dir + w_oper + w_name		&&(Setea variables internas del reporte)
			w_print = .T.
	ENDIF
	IF !EMPTY(p_repfile)
				set safety off

				copy to &p_repfile &p_repfields &p_repfor &p_repfor2 &p_repwhile
	ENDIF

ENDIF

go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
			=rep_form(w_rep)
			p_report=''
endif

RETURN

*=====================================================================================
PROCEDURE	agruasto
*=====================================================================================
PRIVATE w_recno

sele f99c

&&b R.02b Ra๚l
*IF	w_prog = UPPER("impresi๓n por perํodo")		OR			;
*	w_prog = UPPER("impresi๓n por rubro")
*				w_while = " while (ptoi(m.d5peri)+m.d5tipo+m.d5rubr+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+m.d5clie = " +;
*						  "		   ptoi(d5peri)  +  d5tipo+  d5rubr+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie )"		
*				INDEX ON  PTOI(d5peri) + d5tipo + DTOS(d5fech) +	;
*							d5sucu + d5tico  + d5rubr + d5tifo + d5nume + d5clie	to f99c
*ELSE
*	w_while = " while (ptoi(m.d5peri)+m.d5tipo+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+m.d5clie = " +;
*			  "		   ptoi(d5peri)  +  d5tipo+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie)"		
*				INDEX ON  PTOI(d5peri) + d5tipo + DTOS(d5fech) +	;
*							d5sucu + d5tico + d5tifo + d5nume + d5clie 	to f99c
*ENDIF

IF	w_prog = UPPER("impresi๓n por perํodo") 
*				w_while = " while (ptoi(m.d5peri)+m.d5tipo+m.d5rubr+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+m.d5clie = " +;
						  "		   ptoi(  d5peri)+  d5tipo+  d5rubr+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie )"	&& R.17 Mariano
				w_while = " while (ptoi(m.d5peri)+m.d5tipo+m.d5rubr+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+left(m.d5nume+space(17),17)+m.d5clie = " +;
						  "		   ptoi(  d5peri)+  d5tipo+  d5rubr+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie )"	&& R.17 Mariano
				INDEX ON  PTOI(d5peri) + d5tipo + d5rubr + DTOS(d5fech) +	;
							d5sucu + d5tico + d5tifo + d5nume + d5clie	TAG f99cx		&& R.03 Ra๚l
*							d5sucu + d5tico + d5tifo + d5nume + d5clie	to f99c			&& R.03 Ra๚l
ELSE
	IF	w_prog = UPPER("impresi๓n por rubro")
*				w_while = " while (m.d5tipo+ m.d5rubr+ ptoi(m.d5peri)+ DTOS(m.d5fech) +m.d5sucu+ m.d5tico+ m.d5tifo+m.d5nume+m.d5clie = " +;
						  "		     d5tipo+   d5rubr+ ptoi(  d5peri)+ DTOS(  d5fech) +  d5sucu+   d5tico+   d5tifo+  d5nume+  d5clie )"	&& R.17 Mariano
				w_while = " while (m.d5tipo+ m.d5rubr+ ptoi(m.d5peri)+ DTOS(m.d5fech) +m.d5sucu+ m.d5tico+ m.d5tifo+left(m.d5nume+space(17),17)+m.d5clie = " +;
						  "		     d5tipo+   d5rubr+ ptoi(  d5peri)+ DTOS(  d5fech) +  d5sucu+   d5tico+   d5tifo+  d5nume+  d5clie )"	&& R.17 Mariano
				INDEX ON  d5tipo + d5rubr + PTOI(d5peri) + DTOS(d5fech) +	;
							d5sucu + d5tico  + d5tifo + d5nume + d5clie	TAG f99cx		&& R.03 Ra๚l
*							d5sucu + d5tico  + d5tifo + d5nume + d5clie	to f99c			&& R.03 Ra๚l
	ELSE
		IF	w_prog = UPPER("impresi๓n por jurisdicci๓n")		&& R.06 Mariano
*			w_while = " while (m.d5juri+m.d5tipo+ptoi(m.d5peri)+DTOS(d5fech)+m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+m.d5clie = " +;
			  "		   			 d5juri+  d5tipo+ptoi(d5peri)+  DTOS(d5fech) + d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie)"		&& R.06 Mariano	&& R.17 Mariano
			w_while = " while (m.d5juri+m.d5tipo+ptoi(m.d5peri)+DTOS(d5fech)+m.d5sucu+m.d5tico+m.d5tifo+left(m.d5nume+space(17),17)+m.d5clie = " +;
			  "		   			 d5juri+  d5tipo+ptoi(d5peri)+  DTOS(d5fech) + d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie)"		&& R.17 Mariano
			INDEX ON  d5juri+ d5tipo +PTOI(d5peri) + DTOS(d5fech) +	;
						d5sucu + d5tico + d5tifo + d5nume + d5clie 	TAG f99cx				&& R.06 Mariano
		else	&& R.06 Mariano
*			w_while = " while (ptoi(m.d5peri)+m.d5tipo+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+m.d5nume+m.d5clie = " +;
			  "		   ptoi(d5peri)  +  d5tipo+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie)"		&& R.17 Mariano
			w_while = " while (ptoi(m.d5peri)+m.d5tipo+DTOS(d5fech) +m.d5sucu+m.d5tico+m.d5tifo+left(m.d5nume+space(17),17)+m.d5clie = " +;
			  "		   ptoi(d5peri)  +  d5tipo+DTOS(d5fech) +  d5sucu+  d5tico+  d5tifo+  d5nume+  d5clie)"		&& R.17 Mariano
			INDEX ON  PTOI(d5peri) + d5tipo + DTOS(d5fech) +	;
						d5sucu + d5tico + d5tifo + d5nume + d5clie 	TAG f99cx				&& R.03 Ra๚l
*						d5sucu + d5tico + d5tifo + d5nume + d5clie 	to f99c					&& R.03 Ra๚l
		endif	&& R.06 Mariano
	ENDIF
ENDIF
&&b R.02e Ra๚l


&& R.03b Ra๚l
*go top

&& R.04b Ra๚l
*IF	w_prog = UPPER("impresi๓n por rubro")
*				go top
*ELSE
*	=START(ptoi(w_peri), 'f99c')
*ENDIF				

&& R.17b Mariano
*IF	w_prog = UPPER("impresi๓n por fecha")
*					=START(ptoi(w_peri), 'f99c')
*					w_dow = ' AND d5peri = w_peri '
*ELSE
&& R.17e Mariano
	go top
	w_dow = ' '
*ENDIF	&& R.17 Mariano	

&& R.19b Mariano
if w_prog=upper('impresi๓n por Fecha')
	if version()='Visual FoxPro'
		copy to &d0f99.d with cdx type fox2x
	else
		copy to &d0f99.d with cdx
	endif
	use	&d0f99.d in 0 alias f99d order 1
	sele f99c
	go top
endif
&& R.19e Mariano

scatter memvar

&& R.04e Ra๚l

*do while !eof('f99c') and p_cancela
*do while !eof('f99c') and p_cancela AND d5peri = w_peri			&& R.04 Ra๚l
do while !eof('f99c') and p_cancela &w_dow							&& R.04 Ra๚l

&& R.03e Ra๚l

	scatter memvar
	w_recno=recno('f99c')

	** acumulo el comprobante
	IF m.d5tico <> "R"
				sum 	  d5tota,  d5imp1,  d5imp2,  d5imp4,  d5imp5,  d5imp6,  d5imp7,  d5imp8,  d5imp3		;
					to  m.d5tota,m.d5imp1,m.d5imp2,m.d5imp4,m.d5imp5,m.d5imp6,m.d5imp7,m.d5imp8,m.d5imp3		;
				&w_while
	ELSE
		DO retasto
	ENDIF

	SELE f99c
	go w_recno
	gather memvar
	skip in f99c
	IF EOF('f99c') 
			EXIT
	ENDIF
	** deleteo los que sobran

&&b R.02b Ra๚l
*	IF m.d5tico <> "R"
					delete	rest &w_while	
*	ELSE
*		DELE REST FOR	d5tico = "R" AND d5peri = m.d5peri AND d5tipo = m.d5tipo AND		;
*						d5fech = m.d5fech AND d5sucu = m.d5sucu AND d5tico = m.d5tico AND	;
*						d5tifo = m.d5tifo AND d5nume = m.d5nume AND d5clie = m.d5clie
*	ENDIF
&&e R.02e Ra๚l

	go w_recno
	skip in f99c
enddo

return

*========================================================================================
PROCEDURE retasto
*========================================================================================

w_iva	= 0
w_ib	= 0
w_gan	= 0
w_ot	= 0
w_imp	= "m.d5imp"


go w_recno in f99c
w_xxx = "SUM	d5tota	to  w_iva	FOR !EMPTY(d5pere) "  +  w_while
&w_xxx


go w_recno in f99c
w_xxx = "SUM	d5tota	to  w_ib	FOR !EMPTY(d5cpib) "  +  w_while
&w_xxx


go w_recno in f99c
w_xxx = "SUM	d5tota	to  w_gan	FOR !EMPTY(d5cpga) "  +  w_while
&w_xxx


go w_recno in f99c
w_xxx = "SUM	d5tota	to  w_ot	FOR !EMPTY(d5cpot) "  +  w_while
&w_xxx


m.d5tota	= w_iva + w_ib + w_gan + w_ot

IF w_iva <> 0
			m.d5pere	= "X"
			m.d5imp3 = m.d5imp3 + w_iva
			m.d5pere	= " "
ENDIF			

IF w_ib <> 0
			m.d5cpib	= "X"
			DO sumoret
			w_imp		= "m.d5imp" + STR(pn_n,1)
			&w_imp		= &w_imp. + w_ib
			m.d5cpib	= " "
ENDIF

IF w_gan <> 0
			m.d5cpga	= "X"
			DO sumoret
			w_imp	= "m.d5imp" + STR(pn_n,1)
			&w_imp	= &w_imp. + w_gan
			m.d5cpga	= " "
ENDIF

IF w_ot <> 0
			m.d5cpot	= "X"
			DO sumoret
			w_imp	= "m.d5imp" + STR(pn_n,1)
			&w_imp	= &w_imp. + w_ot
			m.d5cpot	= " "
ENDIF


RETURN

*===========================================================================================
PROCEDURE sumoret
*===========================================================================================

DO reten

IF pn_n <> 4					&& Resta m.d5tota porque RETEN lo suma
			REPLACE &pc_imp 	WITH &pc_imp - m.d5tota
			REPLACE &pc_tota	WITH &pc_tota - m.d5tota
ENDIF

RETURN

*=================================================================================
PROCEDURE alta
*===========================================================================================
		if w_vari$"D5NUME.D5TIFO"
			if empty(m.d5rubr)
				m.d5rubr=w_5rubr
			endif
			if empty(m.d5clie)
				m.d5clie=w_5clie
			endif
			if empty(m.d5juri)
				m.d5juri=w_5juri
			endif
			if empty(m.d5ttip)
				m.d5ttip=w_5ttip
			endif
			if empty(m.d5cond)
				m.d5cond=w_5cond
			endif
			if empty(m.d5fech)
				m.d5fech=w_5fech
			endif
*			if empty(m.d5pere) 
*				m.d5pere=w_5pere
*			endif
*			if empty(m.d5cpib) 
*				m.d5cpib=w_5cpib
*			endif
*			if empty(m.d5cpga) 
*				m.d5cpga=w_5cpga
*			endif
*			if empty(m.d5cpot) 
*				m.d5cpot=w_5cpot
*			endif
*			if empty(m.d5ttip)
*				m.d5ttip='0'	&& Grupo
*			endif
			IF EMPTY(m.d5tiop)
							m.d5tiop=ttiop(m.d5tipo,m.d5tico,m.d5tifo)
			ENDIF
			IF EMPTY(m.d5coco)
							m.d5coco=tcoco(m.d5tico,m.d5tifo)
			ENDIF
		endif
RETURN

*================================================================================
PROCEDURE bajaretcv
*===========================================================================================

DO reten

IF pn_n <> 4					&& Resta m.d5tota porque RETEN lo suma
		REPLACE &pc_imp 	WITH &pc_imp - m.d5tota
		REPLACE &pc_tota	WITH &pc_tota - m.d5tota
ENDIF

RETURN

*================================================================================
PROCEDURE retencv
*===========================================================================================

m.d5ttip	= "0"
m.d5tiop	= "E"
m.d5coco	= TCOCO(m.d5tico,m.d5tifo)
m.d5titr	= "N"

DO reten

IF pn_n <> 4					&& Resta m.d5tota porque RETEN lo suma
		REPLACE &pc_imp 	WITH &pc_imp - m.d5tota
		REPLACE &pc_tota	WITH &pc_tota - m.d5tota
ENDIF

RETURN

*===========================================================================================
PROC whensubd
*===========================================================================================

w_retu=.t.
w_vari=varread()
p_curobj=_curobj

if w_vari='W4HOVE'
	w_retu=w_vta
endif

if w_vari='W4HOCO'
	w_retu=w_cpra
endif

if w_vari='WDSUCU' or w_vari='WHSUCU'
	w_retu=w_vta
endif

return(w_retu)

*===========================================================================================
PROC validsubd
*===========================================================================================

PARAMETERS w_show

w_retu=.t.
w_vari=varread()
p_curobj=_curobj

w_dir 	= CURDIR() + "\ARCHIVOS\SDIARIOS\"
w_name	= d0coem + RIGHT(wdperi,2) + LEFT(wdperi,2)

if w_vari='W_VTA' 
	=mchkbox(@w_vta)
endif

if w_vari='W_CPRA'
	=mchkbox(@w_cpra)
endif

if w_vari='W_CPRA' or w_show
	if w_cpra
		wdsucu='    '	&& R.11 Mariano
		whsucu='9999'	&& R.11 Mariano
		show get wdsucu disable
		show get whsucu disable
	else
		show get wdsucu enable
		show get whsucu enable
	endif
endif

IF w_vari = "WDPERI" or w_show

   if lastkey()=p_f4
		do fhelp with 'wdperi','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
   endif
	w_fech	= CTOD('01/' + wdperi)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
				wdperi	= RIGHT(DTOC(w_fech),7)
				SHOW GETS
			   if !seek(ptoi(wdperi),'f04')
		   			if !w_show
					   		wait window 'Perํodo Inexistente'
				   	endif
				   	w_retu=.f.
				   	RETURN .F.
				endif
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(wdperi)
		RETURN .F.
	ENDIF
	w_name	= d0coem + RIGHT(wdperi,2) + LEFT(wdperi,2)

		if !w_show	&& R.07 Mariano
			DO CASE
				CASE !EMPTY(CTOD('31/'+wdperi))
							wlFech	= CTOD('31/'+wdperi)
				CASE !EMPTY(CTOD('30/'+wdperi))
							wlFech	= CTOD('30/'+wdperi)
				CASE !EMPTY(CTOD('29/'+wdperi))
							wlFech	= CTOD('29/'+wdperi)
				OTHERWISE
						wlFech	= CTOD('28/'+wdperi)
			ENDCASE
		endif	&& R.07 Mariano

&& R.05b Ra๚l
	IF w_vta 
				w4hove	=	f04.d4hove 
				w4ulve	=	f04.d4hove 
				IF w4hove = 0
						SKIP -1 IN f04
						w4hove	=	f04.d4hove + 1
						w4ulve	=	f04.d4hove + 1
						SKIP IN f04
				ENDIF			
	ELSE
		w4hove	=	0
		w4ulve	=	0 
	ENDIF
	IF w_cpra 
				w4hoco	=	f04.d4hoco 
				w4ulco	=	f04.d4hoco 
				IF w4hoco = 0
						SKIP -1 IN f04
						w4hoco	=	f04.d4hoco + 1
						w4ulco	=	f04.d4hoco + 1
						SKIP IN f04
				ENDIF			
	ELSE
		w4hoco	=	0
		w4ulco	=	0 
	ENDIF
&& R.05e Ra๚l
endif

IF w_vari = "WHPERI" or w_show

	if lastkey()=p_f4
			do fhelp with 'whperi','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif
	w_fech	= CTOD('01/' + whperi)
	IF EMPTY(w_fech)
			RETURN	.F. 
	ENDIF				
	IF TRDATE('w_fech')
				whperi	= RIGHT(DTOC(w_fech),7)
				SHOW GETS
			   if !seek(ptoi(whperi),'f04')
		   			if !w_show
					   		wait window 'Perํodo Inexistente'
				   	endif
				   	w_retu=.f.
				   	RETURN .F.
				endif
				IF PTOI(wdperi)	> PTOI(whperi)
							WAIT WIND "Perํodo HASTA debe ser mayor a Perํodo DESDE"
							w_retu	= .F.
				ENDIF
	ELSE
		IF !w_show
				WAIT WIND "El perํodo no es correcto"
		ENDIF
		w_retu	= .F.
		_curobj	= objnum(whperi)
		RETURN .F.
	ENDIF
	DO CASE
			CASE !EMPTY(CTOD('31/'+whperi))
							wxfech	= CTOD('31/'+whperi)
			CASE !EMPTY(CTOD('30/'+whperi))
							wxfech	= CTOD('30/'+whperi)
			CASE !EMPTY(CTOD('29/'+whperi))
							wxfech	= CTOD('29/'+whperi)
			OTHERWISE
					wxfech	= CTOD('28/'+whperi)
	ENDCASE
	IF w_vari = "WHPERI"
			whfech	= wxfech
*			wlfech	= wxfech
	ENDIF			
endif

if w_vari='WDFECH' or w_show
	IF !EMPTY(wdfech)
		IF !TRDATE('wdfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
					w_retu	= .F.
					_curobj	= objnum(wdfech)
					RETURN .F.
			ENDIF
		ENDIF
	ENDIF
endif

if w_vari='WHFECH' or w_show
		IF !TRDATE('whfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
					w_retu	= .F.
					_curobj	= objnum(whfech)
					RETURN .F.
			ENDIF
		ENDIF
		if wdfech>whfech
				if !w_show
						wait wind 'Hasta debe ser mayor o igual que desde...'
				endif
				w_retu=.f.
		endif
		IF whfech	> wxfech
				if !w_show
						wait wind 'Fecha Hasta posterior al Perํodo Hasta...'
				endif
				w_retu=.f.
		ENDIF
endif

if w_vari='WDRUBR' or w_show
	if lastkey()=p_f4
		do case
			case (w_vta and w_cpra) or (!w_vta and !w_cpra)
				xpref='RVRC'
			case w_vta
				xpref='RV'
			case w_cpra
				xpref='RC'
		endcase

		IF w_imre	= "S"
					xpref	= xpref + "RR"
		ENDIF

		do fhelp with 'WDRUBR',(xpref),'iwp001','f01','d1cla','d1cla','',(p_strc),'w_retu'

	else
		IF !EMPTY(wdrubr)
						wdrubr=strcero(wdrubr)
		ENDIF
		show get wdrubr
	endif
endif

if w_vari='WHRUBR' or w_show
	if lastkey()=p_f4
		do case
			case (w_vta and w_cpra) or (!w_vta and !w_cpra)
				xpref='RVRC'
			case w_vta
				xpref='RV'
			case w_cpra
				xpref='RC'
		endcase

		IF w_imre	= "S"
					xpref	= xpref + "RR"
		ENDIF

		do fhelp with 'WHRUBR',(xpref),'iwp001','f01','d1cla','d1cla','',(p_strc),'w_retu'
	else
		IF !EMPTY(whrubr)
						whrubr=strcero(whrubr)
		ENDIF
		show get whrubr
	endif
	if wdrubr>whrubr
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDSUCU' or w_show
	IF !EMPTY(wdsucu)
			wdsucu=strcero(wdsucu)
	ENDIF
	show get wdsucu
endif

if w_vari='WHSUCU' or w_show
*	IF !EMPTY(wdsucu)	&& R.10 Mariano
	IF !EMPTY(whsucu)	&& R.10 Mariano
			whsucu=strcero(whsucu)
	ENDIF
	show get whsucu
	if wdsucu>whsucu
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WDJURI' or w_show
			if lastk()=p_f4
					do fhelp with 'WDJURI','JU','iwp001','f01','d1cla','d1cla','',(p_char),'w_retu'
			endif
endif

if w_vari='WHJURI' or w_show
			if lastk()=p_f4
						do fhelp with 'WHJURI','JU','iwp001','f01','d1cla','d1cla','',(p_char),'w_retu'
			endif
			if wdjuri>whjuri
						if !w_show
								wait wind 'Hasta debe ser mayor o igual que desde...'
						endif
						w_retu=.f.
			endif
endif

if w_vari='W_DCLI' or w_show
	if lastk()=p_f4
		IF w_vta
				do fhelp with 'W_DCLI','','iwp002','f02','d2clie','d2nomb','',(p_strc),'w_retu'
		ELSE
			do fhelp with 'W_DCLI','','iwp003','f03','d3prov','d3nomb','',(p_strc),'w_retu'
		ENDIF
		
		if lastkey()=p_esc
			w_dcli=space(35)
			show get w_dcli
		endif
	endif
endif

if w_vari='WDCLIE' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDCLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente',(p_strc),'w_retu'
	else
		wdclie=strcero(wdclie)
	endif
endif

if w_vari='W_HCLI' or w_show
	if lastk()=p_f4
		IF w_vta
				do fhelp with 'W_HCLI','','iwp002','f02','d2clie','d2nomb','',(p_strc),'w_retu'
		ELSE
			do fhelp with 'W_HCLI','','iwp003','f03','d3prov','d3nomb','',(p_strc),'w_retu'
		ENDIF

		if lastkey()=p_esc
			w_hcli=repl('Z',35)
			show get w_hcli
		endif
	endif
	IF UPPER(w_dcli) > UPPER(w_hcli)
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WHCLIE' or w_show

	if lastkey()=p_f4
		do fhelp with 'WHCLIE','','iwp002','f02','d2clie','d2nomb','Cliente inexistente',(p_strc),'w_retu'
	else
		whclie=strcero(whclie)
	endif
	
	if whclie < wdclie
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif

endif

if w_vari='WLFECH' or w_show
		IF !TRDATE('wlfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
					w_retu	= .F.
					_curobj	= objnum(wlfech)
					RETURN .F.
			ENDIF
		ENDIF
		wyfech	= CTOD("01/" + wdperi)
		if wlfech	< wyfech
				if !w_show
						wait wind 'Fecha Listado anterior al inicio del Perํodo Desde...'
				endif
				w_retu=.f.
		endif
endif
SHOW GETS

return(w_retu)

*===========================================================================================
FUNC retnomc
*==========================================================================================

PARAMETERS pd5clie,pd5inte

wnomb	= SPACE(35)
w_cuit	= SPACE(13)
w_domi	= SPACE(13)		&& R.09 Mariano

if pd5clie < '99990'
*			IF w_vta
			IF (d5tipo = "V" AND d5tico <> "R")		OR	 (d5tipo = "C" AND d5tico = "R") OR d5tipo = "X"
					wnomb	= iif(seek(pd5clie,'f02'),f02->d2nomb,'')
					w_cuit	= f02.d2cuit
					w_domi  = allt(f02.d2dire)+'-'+allt(f02.d2loca)+'-'+allt(f02.d2prob)	&& R.09 Mariano
			ENDIF
			IF (d5tipo = "C" AND d5tico <> "R")		OR	 (d5tipo = "V" AND d5tico = "R") OR d5tipo = "Y"
				wnomb	= iif(seek(pd5clie,'f03'),f03->d3nomb,'')
				w_cuit	= f03.d3cuit
				w_domi  = allt(f03.d3dire)+'-'+allt(f03.d3loca)+'-'+allt(f03.d3prob)	&& R.09 Mariano
			ENDIF
else
	wnomb=iif(seek(pd5inte,'f06'),f06->d6nocl,'')
	w_cuit	= f06.d6cuit
endif

IF EMPTY(wnomb)
				wnomb = "No encontrado !!!"	
				wnomb = left(wnomb+space(35),35)	&& R.16 Mariano
ENDIF

*return(wnomb)

RETURN(UPPER(wnomb))

*return(PADR(wnomb,35," "))

*========================================================================================
PROCEDURE ortiva
*==========================================================================================

PARAMETER w_texto

w_acce = 0
IF !USED('f00')
			d0f00='\iw\'+d0coem+'\'+'ivf000'
			w_acce=1
			do while .t.
					fieldvec[1]='f00'
					use (d0f00) in 0 alias f00 &&(Parแmetros generales)
					if .not. neterr()
								exit
					else
						if f_acce>=3	&& tres intentos
									=logout()
									=mquit()
						endif
					endif
					w_acce=w_acce+1
			enddo
ENDIF

w_mensa		= ''
IF EMPTY(d0juce)
			w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +				;
					SPACE(15) + "La empresa no tiene asignada JURISDICCION" + CHR(13) + 	;
					SPACE(15) + "Corregir por men๚ : " + CHR(13) +										;
					SPACE(15) + "                  1บ) ARCHIVOS / TABLAS / JURISDICCIONES" + CHR(13) + 	;
					SPACE(15) + "                  2บ) VARIOS / PARAMETROS / DE LA EMPRESA" + CHR(13) + 	;
					SPACE(15) + "                  3บ) Seleccionar Jurisdicci๓n" + CHR(13) + 				;
					SPACE(15) + "                  4บ) F9"
			w_retu	= .F.
ENDIF

IF EMPTY(d0afip)	AND  !EMPTY(d0juce)
	IF EMPTY(w_mensa)
			w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +				;
					SPACE(15) + "La empresa no tiene asignada JURISDICCION de AFIP" + CHR(13) + 		;
					SPACE(15) + "Corregir por men๚ : " + CHR(13) +										;
					SPACE(15) + "                  1บ) ARCHIVOS / TABLAS / JURISDICCIONES" + CHR(13) + 	;
					SPACE(15) + "                  2บ) VARIOS / PARAMETROS / DE LA EMPRESA" + CHR(13) + ;
					SPACE(15) + "                  3บ) F9 "
			w_retu	= .F.
	ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
					SPACE(15) + "La empresa no tiene asignada JURISDICCION de AFIP" + CHR(13) +			;
					SPACE(15) + "Corregir por men๚ : " + CHR(13) +										;
					SPACE(15) + "                  1บ) ARCHIVOS / TABLAS / JURISDICCIONES" + CHR(13) + 	;
					SPACE(15) + "                  2บ) VARIOS / PARAMETROS / DE LA EMPRESA" + CHR(13) + ;
					SPACE(15) + "                  3บ) F9 "
			w_retu	= .F.
	ENDIF
ENDIF

IF EMPTY(d0gan)
	IF EMPTY(w_mensa)
			w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) + 		; 	
					SPACE(15) + "No se puede determinar si es Persona Fํsica o Jurํdica para exportar ganancias" + CHR(13) + ;
					SPACE(15) + "Corregir por men๚ : " + CHR(13) +											;
					SPACE(15) + "                  1บ) VARIOS / PARAMETROS / DE LA EMPRESA" + CHR(13) +		;
					SPACE(15) + "                  2บ) Ingresar F o J en el campo de GANANCIAS" + CHR(13) +	;
					SPACE(15) + "                  3บ) F9 "
			w_retu	= .F.
	ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
				 	SPACE(15) + "No se puede determinar si es Persona Fํsica o Jurํdica para exportar ganancias" + CHR(13) + ;
				  	SPACE(15) + "Corregir por men๚ : " + CHR(13) +											;
				  	SPACE(15) + "                  1บ) VARIOS / PARAMETROS / DE LA EMPRESA" + CHR(13) +		;
				  	SPACE(15) + "                  2บ) Ingresar F o J en el campo de GANANCIAS" + CHR(13) +	;
				  	SPACE(15) + "                  3บ) F9 "
			w_retu	= .F.
	ENDIF
ENDIF

IF f00.p0prv1 <> 1	AND f00.p0prv2 <> 1	AND f00.p0prv3 <> 1
		d0ibvta		= .T.
		w_retu	= .F.
		IF EMPTY(w_mensa)
				w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +			;
					 	SPACE(15) + "NO TIENE DEFINIDO INGRESOS BRUTOS EN VENTAS" 
		ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
				 	SPACE(15) + "NO TIENE DEFINIDO INGRESOS BRUTOS EN VENTAS" 
		ENDIF
ENDIF

IF f00.p0prc1 <> 1	AND f00.p0prc2 <> 1	AND f00.p0prc3 <> 1
		d0ibcpra	= .T.
		w_retu	= .F.
		IF EMPTY(w_mensa)
				w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +			;
		 				SPACE(15) + "NO TIENE DEFINIDO INGRESOS BRUTOS EN COMPRAS"
		ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
		 			SPACE(15) + "NO TIENE DEFINIDO INGRESOS BRUTOS EN COMPRAS"
		ENDIF
ENDIF

IF f00.p0prv1 <> 2	AND f00.p0prv2 <> 2	AND f00.p0prv3 <> 2
		d0ganvta	= .T.
		w_retu	= .F.
		IF EMPTY(w_mensa)
				w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +			;
		 				SPACE(15) + "NO TIENE DEFINIDO GANANCIAS EN VENTAS"
		ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
		 			SPACE(15) + "NO TIENE DEFINIDO GANANCIAS EN VENTAS"
		ENDIF
ENDIF

IF f00.p0prc1 <> 2	AND f00.p0prc2 <> 2	AND f00.p0prc3 <> 2
		d0gancpra	= .T.
		w_retu	= .F.
		IF EMPTY(w_mensa)
				w_mensa =	"EMPRESA : " + d0CoEm + "  " + ALLT(d0emp) + CHR(13) + CHR(13) +			;
		 				SPACE(15) + "NO TIENE DEFINIDO GANANCIAS EN COMPRAS"
		ELSE
			w_mensa = w_mensa + CHR(13) + CHR(13) +	;
		 			SPACE(15) + "NO TIENE DEFINIDO GANANCIAS EN COMPRAS"
		ENDIF
ENDIF

IF !EMPTY(w_mensa)		AND !EMPTY(w_texto)
		w_mensa = w_mensa + CHR(13) + CHR(13) + CHR(13)
		if !used('SSF090')
					use ssf090 in 0 alias ssf090
		ENDIF

		=net_appe('ssf090')
		=set_alia('ssf090')

		REPLA errdate WITH DATE(), errtime WITH TIME(), errdata WITH UPPER(w_texto),	;
							listing WITH w_mensa

		=rest_alia()
ENDIF

IF w_acce <> 0
			SELE f00
			USE
ENDIF

RETURN

&& R.08b Ra๚l
*==========================================================================================
PROCEDURE titsubd
*==========================================================================================

	IF w_reptit = ALLT(f00.p0tit1)
			w_subt = ALLT(f00.p0subt1)
	ELSE			
		w_subt = ALLT(f00.p0subt2)
	ENDIF
	
	IF f00.p0peri = 'S' AND f00.p0linperi = '4'	
		IF EMPTY(w_reptit)
					w_reptit = t_leye + mes(ctod('01/'+d5peri))+' de '+right(d5peri,4)
		ELSE					
			w_reptit	= w_reptit + ' - ' + t_leye + mes(ctod('01/'+d5peri))+' de '+right(d5peri,4)
		ENDIF
	ENDIF					
	
	IF f00.p0peri = 'S' AND f00.p0linperi = '5'	
		IF EMPTY(w_subt)
					w_subt	= t_leye + mes(ctod('01/'+d5peri))+' de '+right(d5peri,4)
		ELSE
			w_subt	= w_subt + ' - ' + t_leye + mes(ctod('01/'+d5peri))+' de '+right(d5peri,4)
		ENDIF
	ENDIF					

RETURN

*==========================================================================================
&& R.08e Ra๚l

*==========================================================================================
FUNC invarpub	&& inicializa variables PUBLICas
*==========================================================================================

parameters w_util

if w_util	&& estoy en el util
	private wVersExe,wVersPic,wVersRel,wVerxExe
	private wacla,walet,wncla, w_sys, w_ver, w_rel, w_verel, w_leer, w_actver
else		&& estoy en el sistema
	PUBLIC wVersExe,wVersPic,wVersRel,wVerxExe
	PUBLIC wacla,walet,wncla, w_sys, w_ver, w_rel, w_verel, w_leer, w_actver
endif

PUBLIC ARRAY w_veract(100)

w_actver	= .F.
FOR n = 1 TO 100
		w_veract(n) = " "
ENDFOR

w_sys   ='SISTEMA DE IVA Compras-Ventas'

walet=Space(4)
wVersPic='4.18'
wVersExe=strtran(wVersPic,'.','')

wVersRel=' 33 '+lib()
*          Sy    LN

w_ver = 'Ver.: ' + wVersPic + wVersRel + " R02A"
*											  ?
*										 	  |____ Acแ debe ir "A" xa que fuerce la actualizaci๓n
*													de estrucutras y datos de sistema sin cambiar
*													de versi๓n, caso contrario poner " " (blanco).

*==================================================================
*===> ATENCION !!!!!!!!!!!!!!!!!!!!!!
*			Cuando se pase a R00, asterisquear la carga del vector

*w_veract(1x)	= "A"			&& R 1x - 1
w_veract(02)	= "A"			&& R.25 Mariano
w_veract(03)	= "A"			&& R.26 Mariano

*==================================================================

w_rel = STRTRAN(SUBSTR(w_ver,21,1)," ","0")

*===>	.T. = Abre pantalla de DOCUMENTACION VERSION
*		.F. = NO Abre pantalla de DOCUMENTACION VERSION

w_leer = .T.
*w_leer = .F.

*===> ASI XA Q CUANDO COMPARE CON NF_VERS.
wVerxExe=wVersExe+strtran(wVersRel,' ','')

*===> ESTO PASA A LA CHECKVER		
*wVerxExe	= wVersExe + STRTRAN(wVersRel,' ','') + SUBSTR(w_ver,19,2)

return(wVerxExe)

*=======================================================================================
Function chpw
*==========================================================================================

if p_repout='Impresora' or (p_repout='Archivo' and !p_reptype='TXT   Pantalla')
 return .t.
else
 return .f.
endif

*=======================================================================================


**********************************************************
* Puesto adrede por si desaparece algun prg del proyecto *
* Mantener esto al final del archivo. Y agregar nuevos   *
*														 *
* Y MANTENER EL ORDEN !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	 *
*---------------------------------------------------	 *
**********************************************************
do cgp001c
do cgp001cl
do cgp004
do cgp903

do iwp001
do iwp001l
do iwp001t
do iwp002
do iwp002l
do iwp003
do iwp003l
do iwp004
do iwp004d
do iwp004l
do iwp005a
do iwp005c
do iwp005e
do iwp005g
do iwp005i
do iwp005j
do iwp005l
do iwp005m
do iwp005o
do iwp005p
do iwp005r
do iwp005s
do iwp005t
do iwp005u
do iwp005v
do iwp008
do iwp008l
do iwp009
do iwp009l
do iwp010
do iwp010e
do iwp010l
do iwp010t
do iwp013
do iwp013l
do iwp013m
do iwp013ml
do iwp014
do iwp014l
do iwp015
do iwp015l
do iwp016
do iwp016c
do iwp016l
do iwp017
do iwp017p
do iwp020
do iwp021
do iwp021l
do iwp022
do iwp022d
do iwp022l
do iwp023
do iwp023l
do iwp024
do iwp024l
do iwp025
do iwp025l
do iwp066
do iwp066l
do iwp093
do iwp094
do iwp095
do iwp096
do iwp097
do iwp860p
do iwp861
do iwp862
do iwp863
do iwp865
do iwp866
do iwp900c
do iwp900p
do iwp900r
do iwp900s
do iwp940
do iwp950
do iwp980
do iwp999
do iwpver

do library

do ssp001
do ssp001u
do ssp003
do ssp003l
do ssp090


*===> usan la pantalla iwscpa2
do iwpiva				
do iwpsicol				
do iwpifere				
do iwpib
do iwpsipot

*===> usan la pantalla iwsvta1
do iwprciba				

*===> usan la pantalla iwsvta2
do iwpicore				

do iwpfisic

do iwp900f

do iwpadib

do iwpsicow	&& sicol web

do iwpjurid	&& Ganancias personas juridicas-sociedades

do iwpsarep	&& IIBB Mendoza Perc./Ret. Practicadas (SAREPE WEB)

do iwp900uh
do iwp900ur

do iwp862ri	&& R.13 Mariano
do iwp866ri	&& R.13 Mariano

do iwp005in	&& R.16 Mariano
do iwp004in	&& R.16 Mariano

do iwp005er	&& R.19 Mariano
do iwpferew	&& R.19 Mariano
do iwp006in	&& R.19 Mariano

do iwsver.spr	&& R.24 Mariano

do iwparib	&& R.25 Mariano