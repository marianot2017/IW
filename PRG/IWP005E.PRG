*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP005E.PRG -> Emisor de comprobantes
*)-H
*)-H Release		 : 01
*)-H Fecha de inicio : 
*)-H Fecha de fin    : 
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*

if empty(parameters())
	w_call=.t.
	w_pref=''
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_pfile='05e'
w_alias='f05'
w_title=upper(prompt())

PRIVATE w5alia
w5Alia=''
wMenu =.f.
wabm  =.f.
wTiCoInte='' &&(A/B/C/*)
wwTiFo=iif(d0CoIm='1','ABME ','CE ') &&(Tipos de comprobantes generables)
w_grupo=.f.

*) Apertura de archivos--------------------------------------------------------

do case
	case type('m.d5tipo')=='U'
		*) Fue llamado por el menu ivp600		
		wMenu=.t.
		wGene=.f.
		=abrefex()
		
		fieldvec[1]='FFO'
		fieldvec[2]='F00'		
		fieldvec[3]='F01'		
		fieldvec[4]='F02'
		fieldvec[5]='F04'
		fieldvec[6]='F05'
		fieldvec[7]='F06'
		fieldvec[8]='F07'
		fieldvec[9]='F23'		
		
		n0ffo='\IW\FFO'
      use (n0ffo) in 0 alias 'ffo'
      use (d0f00) in 0 alias 'f00'					  	&&(Parameteros)
		use (d0f01) in 0 alias 'f01' order 'd1tip'	&&(Tablas)
      use (d0f02) in 0 alias 'f02' order 'd2clie'	&&(Clientes)
      use (d0f04) in 0 alias 'f04' order 'd4peri' 	&&(Periodos)
      use (d0f05) in 0 alias 'f05' order 'd5peri' 	&&(Comprobantes)
      use (d0f06) in 0 alias 'f06' order 'd6inte' 	&&(Arch.Alternativo)
      use (d0f07) in 0 alias 'f07' order 'd7inte' 	&&(Arch.Textos)
		use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)
      
      if neterr()
			pop key
			pop menu _msysmenu
			fieldvec=''
			=closall()
			return
		endif
		
		wTiCo='F'
      wF3  =.f.
      sele (w_alias)
      
	case !type('wgene')=='U'
	  w_title=upper('Impresi๓n de comprobantes')
   	*) Fue llamado por el generador 5g
      =abrefex()
      wTiCo='F'
      wF3  =.f.

   otherwise
	  w_title=upper('Impresi๓n de comprobantes')
      *) Fue llamado por el abm 5v
      wabm =.t.
      wGene=.f.
      wF3  =.f.
      if empty(m.d7memo)
         =advsuave('Comprobante sin Texto...','Informaci๓n')
         return
      endif
      w5Alia=alias()
      =abrefex()
      sele (w5Alia)
*      =f_set('A'+f05->d5TiFo)
*      wFormAnt='A'+f05->d5TiFo
      =f_set(f05.d5tico+f05.d5TiFo)
      wFormAnt=f05.d5tico+f05.d5TiFo
      wTiCoInte=f05->d5TiCo
      wTiCo=itoe(wTiCoInte)
      
endcase

sele 0
create dbf &d0f99.c (d99expr c(161),d99font c(1),d99grup l(1))
use &d0f99.c alias f99c exclusive

*)-----------------------------------------------------------------------------

sele (w_alias)

do case
   case wMenu
		*) Fue llamado por el Menu
      go bott in f04
      wPeri =f04->d4peri
      wdnume=repl(' ',d0ptcv)+'/'+repl(' ',len(d5nume)-d0ptcv-1)
      whnume=repl('9',d0ptcv)+'/'+repl('Z',len(d5nume)-d0ptcv-1)
      wTiFo = subs(wwTiFo,1,1)
      wdSucu='00000'	&& R.01 Mariano (Agrande 1 digito)
      whSucu='99999'	&& R.01 Mariano (Agrande 1 digito)
	case wGene
   	wdSucu=wSucu
      whSucu=wSucu
   case wabm
	  wPeri	=m.d5peri
      wdnume=m.d5nume
      whnume=m.d5nume
      wTiFo=m.d5TiFo
      wdSucu=m.d5Sucu
      whSucu=m.d5Sucu
endcase

do ssm998.mpr
do iws005e.spr

if db_rlock('f00')
	repl f00->p0FaRe with wTiCo
	unlock in f00
endif

=f_rel()

do case
   case wMenu
   	=closall()
   case wAbm
      sele fex
      use
      sele (w5Alia)
   case wGene
      sele fex
      use
endcase

pop key
pop menu _msysmenu

if used('f99c')
	sele f99c
	use
endif
dele file &d0f99.c.dbf

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC when05e
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

return (type('m.d5tipo')=='U')


*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid05e
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()

if w_vari='WPERI' or w_show

	if lastkey()=p_f4
		do fhelp with 'WPERI','','iwp004','f04','d4peri','d4peri','',(p_char),'w_retu'
	endif

	wkperi = PtoI(wperi)

	if ! seek(wkperi,'f04')
		if !w_show
			wait window 'Perํodo Inexistente'
		endif
		w_retu=.f.
	endif
endif


if w_vari='WTIFO' or w_show
	if !wtifo$'ABCME '
		if !w_show
			wait wind 'Opciones vแlidas: A,B,C,M,E, '
		endif
		w_retu=.f.
	endif
endif

if w_vari='WTICO' or w_show
	if !wtico$'FDC'
		if !w_show
			wait wind 'Opciones vแlidas: F,D,C'
		endif
		w_retu=.f.
	endif
endif

if !wF3
	wdSucu=Strcero(wdSucu)
   whSucu=Strcero(whSucu)
   show get wdsucu
   show get whsucu
endif

if w_vari='WHSUCU' or w_show	
	if wdsucu>whsucu
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if !wGene .and. !wF3
	*) No fue llamado por el Generador
   wdNume=StrVent(wdNume,09,65)
   whNume=StrVent(whNume,10,65)
endif

if w_vari='WHNUME' or w_show	
	if wdnume>whnume
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

wTiCoInte=etoi(wTiCo)

return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05e
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons05e0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons05e0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid05e(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

sele f99c
zap

set century off

sele (w_alias)

wBreak=.f.
wTiCoAnt=wTiCoInte &&(Agregado)
if start(PtoI(wperi)+'V'+wdSucu+wtifo+wticointe+wdnume,'f05')
	do List05e	
	do repdest &&(Setea variables internas del reporte)
	sele f99c
	go top	
	if !empty(p_repclaus) &&(debe ser preview o to printer)
		p_report='iwr005e'
*		SET STEP ON
*		ACTI WIND DEBUG
		
		=rep_form()
		p_report=''
	endif	
	do represet &&(Resetea estado)

else

   =advsuave('No existen Comprobantes...','Informaci๓n')

endif
wTiCoInte=wTiCoAnt

_curobj=1

set century on

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC List05e &&(Ciclo File Comprobantes)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
t_cara = 80
w0doon = ''
w0doof = ''

wprime = .t.
*if !wabm
   wFormAnt=''
*endif

do while (f05.d5peri+f05.d5tipo)=wperi+'V';
	and (f05.d5Sucu+f05.d5tifo+f05.d5Nume) <= whSucu+wtifo+whNume;
   and !eof('f05') and p_cancela   
  	
  	do case
      case f05->d5TiFo <> wTiFo or f05->d5Nume < wdNume
         skip in f05
         loop
      case wTiCoInte='*'
      case f05->d5TiCo<>wTiCoInte
         skip in f05
         loop
   endcase
   
   =seek(f05->d5Inte,'f07')
   wmemo=f07->d7memo
   
   if empty(wmemo)
      skip in f05
      loop
   endif
   
*   if !('A'+f05.d5TiFo)==wFormAnt
*      if f_set('A'+f05.d5TiFo) &&(antes d5tico)
*         wFormAnt='A'+f05.d5TiFo
*      else
*         f_cacp=0
*      endif
*   endif
   if !(f05.d5tico+f05.d5TiFo)==wFormAnt
      if f_set(f05.d5tico+f05.d5TiFo)
         wFormAnt=f05.d5tico+f05.d5TiFo
      else
         f_cacp=0
      endif
  endif

	=seek('RV'+f05.d5rubr,'f01')
	=seek(f05.d5alic,'f23')
	if !found('f23')
		d0ivan=0
		d0ivaa=0
	else
		d0ivan=f23.d23alic
		d0ivaa=f23.d23acre
	endif
	
   for ii=1 to f_cacp
  	   if p_cancela
   	   w_grupo=!w_grupo
		   do impre05e
	   else
	   	  exit
	   endif
   endfor
	 
   if !p_cancela
   		exit
   endif
   
   skip in f05

enddo
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Impre05e &&(Imprimir UN Cbte)
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

*)ENCABEZAMIENTO

=seek(f05->d5clie,'f02')
if f05->d5clie >= '99990'
   =seek(f05->d5Inte,'f06')
   w5nocl = f06->d6nocl
   w5cuit = f06->d6cuit
   w5dire = f06->d6dire
   w5copo = f06->d6copo
   w5loca = f06->d6loca
else
   w5nocl = f02->d2nomb
   w5cuit = f02->d2cuit
   w5dire = f02->d2dire
   w5copo = f02->d2copo
   w5loca = f02->d2loca
endif

w0Len  = ''
w0Ini  = ''
w0doon = ''
w0doof = ''
*if wsali<>'P'
*   w0Ini  = d0Ini
*   w0Len  = &d0clfo
*   w0doon = d0doon
*   w0doof = d0doof
*endif

=f_cabe()

*) DETALLE

wLine=1
do while wLine <= f_cal_det .and. !wBreak
   =f_deta(wLine)
   wLine=wLine+1
enddo

*) PIE

Priv wcar,wanc,wtoc,i,in

w5imp1 = f05->d5imp1  && grav
w5imp2 = f05->d5imp2  && no grav
w5imp4 = f05->d5imp4  && iva
w5imp5 = f05->d5imp5  && iva ni
w5imp3 = f05->d5imp3  && iva ret
w5imp6 = f05->d5imp6  && otros conc 1
w5imp7 = f05->d5imp7  && otros conc 2
w5imp8 = f05->d5imp8  && otros conc 3
w5tota = f05->d5tota

do case
   case d0CoIm='1'.and. f02->d2tipo$'12'
      *leye = 'Neto Gravado'
   other
      *leye = 'Subtotal'
      w5imp1 = w5imp1 + w5imp2 + w5imp4 + w5imp5 + w5imp3
      *         grav    no grav  iva      iva ni   iva ret
      stor 0 to w5imp2,w5imp4,w5imp5,w5imp3
endcase

=f_pie()

v_reg_ini=recno('f99c')

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC abrefex
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
n0fex='\IW\fex'
do while .t.
	fieldvec[1]='fex'
	use (n0fex) in 0 alias 'fex' order 'd44clav'
	if !neterr()
		exit
	endif
	fieldvec=''
enddo
sele fex
go top
return('')

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC etoi
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para externo
priv interno
do case
   case externo='F'
      interno='A'
   case externo='D'
      interno='B'
   case externo='C'
      interno='C'
   case externo='T'
      interno='*'
   otherwise
      interno=externo
endcase
return(interno)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC itoe
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para interno
priv externo
do case
   case interno='A'
      externo='F'
   case interno='B'
      externo='D'
   case interno='C'
      externo='C'
   case interno='*'
      externo='T'
   otherwise
      externo=interno
endcase
return(externo)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC StrToScr
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
para wString
priv wScreen,i
wScreen=''
for i=1 to len(wString)
   wScreen=wScreen+subs(wString,i,1)+chr(143)
next
return(wScreen)

***puesto adrede para que incluya el report al proyecto**
report form iwr005e
