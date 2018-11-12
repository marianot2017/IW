*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ)
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005G.PRG -> Generacion de Facturas
*)-H release 		 : 02
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
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.

*) Datos del proceso-----------------------------------------------------------

w_pfile='05g'
w_alias='f09'
w_title=upper(prompt())

wperi=''
wsucu='00000'	&& R.02 Mariano (Agrande 1 digito)
wtifo='A'
wtipo=''
wfech={}
wdclie='00000'
whclie='99999'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1 ]='FFO'
fieldvec[2 ]='F00'
fieldvec[3 ]='F01'
fieldvec[4 ]='F02'
fieldvec[5 ]='F04'
fieldvec[6 ]='F05'
fieldvec[7 ]='F06'
fieldvec[8 ]='F07'
fieldvec[9 ]='F08'
fieldvec[10]='F09'
fieldvec[11]='F14'
fieldvec[12]='F20'
fieldvec[13]='F23'

n0ffo='\IW\ffo'
use (n0ffo) in 0 alias 'ffo'
use (d0f00) in 0 alias 'f00' 					   exclu &&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   exclu &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  exclu &&(Clientes)
use (d0f04) in 0 alias 'f04' order 'd4peri'  exclu &&(Periodos)
use (d0f05) in 0 alias 'f05' order 'd5peri'  exclu &&(comprobantes)
use (d0f06) in 0 alias 'f06' order 'd6inte'  exclu &&(Arch.Alternativo)
use (d0f07) in 0 alias 'f07' order 'd7inte'  exclu &&(textos)
use (d0f08) in 0 alias 'f08' order 'd8codi'  exclu &&(Facturas tipo)
use (d0f09) in 0 alias 'f09' order 'd9clie'  exclu &&(Facturas tipo x cliente)
use (d0f14) in 0 alias 'f14' order 'd14tico' exclu &&(Facturas tipo x cliente)
use (d0f20) in 0 alias 'f20' order 'd20tipo' exclu &&(Tipos impos. x periodos)
use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
*)-----------------------------------------------------------------------------

wdeta =f00->p0deta
w5tipo=''
wgene =.t. &&(Flag que tiene en cuenta 005e)
wejer =right(w0fipre,2)

go bott in f04
wperi=f04->d4peri

do case
case !empty(ctod('31/'+wperi))
	wfech=ctod('31/'+wperi)
case !empty(ctod('30/'+wperi))
	wfech=ctod('30/'+wperi)
case !empty(ctod('29/'+wperi))
	wfech=ctod('29/'+wperi)
other
	wfech=ctod('28/'+wperi)
endcase

do ssm998.mpr
do iws005g.spr

pop key
pop menu _msysmenu
release wcanu,w_n446,k,n,o
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05g
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05g0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05g0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if seek(PtoI(wperi),'f04')
	*) Se Guardan los tipos de Formularios que se pueden Generar
	wwTiFo=iif(f04->d4CoIm='1','ABME ','CE ')
else
	wait wind 'Perํodo de IVA Inexistente...'
	return
endif

w5tico='A'
w5sucu=wSucu
w5tifo=wTiFo

do case
case f04->d4esve='C'
	wait wind 'Perํodo de IVA Cerrado.'
	return
case empty(wTipo)
	wait wind 'Tipo de Generaci๓n no Indicado.'
	return
case !wTiFo$wwTiFo
	wait wind 'No se pueden generar estos tipos de formularios.'
	return
case !seek(w5tico+w5sucu+w5tifo,'f14')
	wait wind 'Numeraci๓n Inexistente.'
	return
otherwise
endcase

if sino('Confirma el Proceso ?.')='N'
	return
endif

*) p0 Ultima fActura Ventas
*) wNu00='f00->p0uav'+wTiFo
*) wNuAn= &wNu00

do LeeNume &&(Lee Numeros correpondientes a formularios segun sucursal y Cpte)
wPrim=MasUno(TomaNume(w5tico+w5sucu+w5tifo))

wCant=0
=start(wdclie,'f09')
do while f09->d9clie<=whclie and !eof('f09')
	
	w2tipo=FTiImP(f09->d9clie,wperi,'C')
	do case
	case !wTipo $ f09->d9tipo
	case !seek(f09->d9Clie,'f02')
		wait wind 'Cliente '+f09->d9Clie+' Inexistente.'
	case !wTiFo $ iif(w2tipo$'12','ACM ','BCE ')
	case !seek(f09->d9codi,'f08')
		wait wind 'Factura Tipo '+f09->d9Codi+' Inexistente.'
	case !seek('JU'+f02->d2juri,'f01')
		wait wind 'Jurisdicci๓n Inexistente.'
	otherwise
	
		if begintran()
		
			=net_appe('f05')
			w5Nume=MasUno(TomaNume(w5tico+w5sucu+w5Tifo))
			wait wind 'Cliente '+f09->d9clie+' Tipo '+f09->d9codi+' Nro. ' +w5Nume nowait
			d0Inte=right(str(1000001+val(d0Inte),7),6)
			
			sele f05
			repl f05->d5peri with wperi;
				,f05->d5empr with d0coem;
				,f05->d5tipo with 'V';
				,f05->d5Sucu with wSucu;
				,f05->d5tico with 'A';
				,f05->d5nume with w5Nume;
				,f05->d5Inte with d0Inte;
				,f05->d5TiFo with wTiFo;
				,f05->d5juri with f02->d2juri;
				,f05->d5fech with wfech;
				,f05->d5cond with '1';
				,f05->d5ttip with '0';
				,f05->d5coco with TCoCo('A',wTifo);
				,f05->d5tiop with TTiOp('V','A',wTifo);
				,f05->d5rubr with f09->d9rubr;
				,f05->d5clie with f09->d9clie;
				,f05->d5imp1 with f08->d8imp1;
				,f05->d5imp2 with f08->d8imp2;
				,f05->d5imp3 with f08->d8imp3;
				,f05->d5imp4 with f08->d8imp4;
				,f05->d5imp5 with f08->d8imp5;
				,f05->d5imp6 with f08->d8imp6;
				,f05->d5imp7 with f08->d8imp7;
				,f05->d5imp8 with f08->d8imp8;
				,f05->d5tota with f08->d8tota;
				,f05->d5coim with f04->d4coim;
				,f05->d5tiim with w2tipo;
				,f05->d5pere with '000'
				
				=net_appe('f07')
				sele f07
				repl f07->d7Inte with d0Inte;
					,f07->d7memo with f08->d7memo
			
			
				if f09->d9clie>'99989'
					=net_appe('f06')
					sele f06
					repl f06->d6Inte with d0Inte
					repl f06->d6nocl with f02->d2Nomb;
						,f06->d6cuit with f02->d2cuit;
						,f06->d6dire with f02->d2dire;
						,f06->d6copo with f02->d2copo;
						,f06->d6loca with f02->d2loca
				endif
				
				*) Guarda Ultimo Nฃmero && (&wNu00 with left(f05->d5nume,d0ptcv)
				=GuarNume(w5tico+w5sucu+w5tifo)
				
				*) Graba Ultimo Nฃmero Int
				sele f00
				repl f00->p0Inte with d0Inte

				if seek(PtoI(f05->d5peri),'f04')
					sele f04
					repl f04->d4vtot with f04->d4vtot + f05->d5tota;
						,f04->d4vim1 with f04->d4vim1 + f05->d5imp1;
						,f04->d4vim2 with f04->d4vim2 + f05->d5imp2;
						,f04->d4vim3 with f04->d4vim3 + f05->d5imp3;
						,f04->d4vim4 with f04->d4vim4 + f05->d5imp4;
						,f04->d4vim5 with f04->d4vim5 + f05->d5imp5;
						,f04->d4vim6 with f04->d4vim6 + f05->d5imp6;
						,f04->d4vim7 with f04->d4vim7 + f05->d5imp7
				endif
				
				wCant=wCant+1
			
			=endtran()
		endif

	endcase
	
	skip in f09
	
enddo

wait clear

if empty(wCant)
	=advsuave('No se Gener๓ Ninguna Factura','Informaci๓n')
else
	do GrabNume
	wdnume=wPrim
	whnume=strVent(TomaNume(w5tico+w5sucu+w5Tifo))
	=advsuave('Se generaron desde '+wdNume+' hasta '+whNume,'Informaci๓n')

	if sino('Desea Imprimirlas ?.')='S'
		sele f09
		use
		m.d5tipo=''
		do iwp005e
		do while .t.
			fieldvec[1]='f09'
			use (d0f09) in 0 alias 'f09' order 'd9clie'  exclu &&(Facturas tipo x cliente)
			if !neterr()
				exit
			endif
		enddo
	endif

endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid05g
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='WPERI' or w_show
		if lastkey()=p_f4
			do fhelp with 'WPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
		endif
		w_fech	= CTOD('01/' + wperi)
		IF EMPTY(w_fech)
						RETURN	.F. 
		ENDIF

		IF TRDATE('w_fech')
				wperi	= RIGHT(DTOC(w_fech),7)
				SHOW GET wperi
		ELSE
			IF !w_show
					WAIT WIND "El perํodo no es correcto"
					w_retu	= .F.
					_curobj	= objnum(wperi)
					RETURN .F.
			ENDIF
		ENDIF
		if ! seek(ptoi(wperi),'f04')
			if !w_show
				wait window 'Perํodo Inexistente'
			endif
			w_retu=.f.
		else
			if f04->d4esco='C'
				wait window 'Perํodo Cerrado'
				if !w_show
					w_retu=.f.
				endif
			endif
		endif
endif

if w_vari='WDCLIE' or w_show
	if lastkey()=p_f4
		do fhelp with 'WDCLIE','','iwp002','f02','d2clie','d2clie','Cliente inexistente.',(p_strc),'w_retu'
	else
		wdclie=strcero(wdclie)
		show get wdclie
	endif
endif

if w_vari='WHCLIE' or w_show
	if lastk()=p_f4
		do fhelp with 'whclie','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	else
		whclie=strcero(whclie)
		show get whclie
	endif
	if wdclie>whclie
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='WFECH' or w_show
		IF !TRDATE('wfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
					w_retu	= .F.
					_curobj	= objnum(wfech)
					RETURN .F.
			ENDIF
		ELSE
			IF wperi <> RIGHT(DTOC(wfech),7)
						IF !w_show
								WAIT WIND "La fecha no corresponde al perํodo"
								w_retu	= .F.
								_curobj	= objnum(wfech)
								RETURN .F.
						ENDIF
			ENDIF
		ENDIF
endif

if w_vari='WSUCU' or w_show
	wsucu=strcero(wsucu)
	show get wsucu
endif

return(w_retu)

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Proc LeeNume
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* * *  Lee numeraciขn de archivo 14 segun Sucursal
sele f14
set filter to d14sucu=w5sucu .and. d14tico=w5tico
go top in f14

publ wcanu,w_n446
count to wcanu
if !empty(wcanu)
   public array k[wcanu],n[wcanu],o[wcanu]
   go top in f14
   indi=0
   do while ! eof('f14')
      indi=indi+1
      k[indi]=d14tico+d14sucu+d14tifo
      n[indi]=d14ulnu
      o[indi]=d14n446
      skip in f14
   enddo
else
   publ k[1],n[1],o[1]
   k[1]=spac(6)
   n[1]=spac(8)
   o[1]=spac(8)
endif

w_n446=spac(8)

sele f05
* * * ----------------------------
return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
func TomaNume
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* Toma numero de la variable correspondiente

para t_key
priv t_nume,t_a
w_n446=spac(8)
t_nume=space(12)
if !empty(wcanu)
   t_a=ascan(k,t_key)
   if !empty(t_a)
      t_nume=n[t_a]
      w_n446=o[t_a]
   endif
endif
*t_nume= right( t_nume,d0ptcv)
t_nume= strVent(t_nume)
return t_nume

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func GuarNume
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* Guarda numero de la variable correspondiente

para t_key
priv t_a
if !empty(wcanu)
   t_a=ascan(k,t_key)
   if !empty(t_a)
      n[t_a]=w5nume
   endif
endif
return ''

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Proc GrabNume
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* Se Graban los ultimos numeros de comprobantes

priv g_i
for g_i=1 to wcanu
    if seek(k[g_i],'f14')
       =db_rlock('f14')
       sele f14
       repl d14ulnu with n[g_i]
       go recno() in f14
       sele (w_alias)
       unlock in f14
    endif
next
return

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func FTiImP
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
* Retorna Categoria Impositiva segun cliente-proveedor y periodo
para f_clpr,f_peri,f_tipo
priv f_ret,f_rec

f_peri=PtoI(f_peri)

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
         if seek(f_clpr,'f02')
            f_ret=f02->d2tipo
         endif
         go f_rec in f02
      else
      	if !eof('f03')
	         f_rec=recno('f03')
	      else
	      	f_rec=recno('f03')-1
	      endif
         if seek(f_clpr,'f03')
            f_ret=f03->d3tipo
         endif
         go f_rec in f03
      endif
   endif
endif
return f_ret

*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func FTiImX
*)ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para ff_tipo
priv ff_clpr,ff_ret
ff_clpr=iif(ff_tipo='C',f02->d2clie,f03->d3prov)

if ff_tipo='C'
	sele f02
	set order to 1
	sele (w_alias)
else
	sele f03
	set order to 1
endif

ff_ret=FTiImP(ff_clpr,f04->d4peri,ff_tipo)

if ff_tipo='C'
	sele f02
	set order to 2
	sele (w_alias)
else
	sele f03
	set order to 2
endif
return ff_ret
