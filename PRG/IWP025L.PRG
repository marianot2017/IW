*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP025L.PRG -> Listador de Perc./Ret. Ingresos Brutos
*)-H
*)-H Fecha de inicio : 
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.

*) Variables del report--------------------------------------------------------

w_pfile='25l'
w_title=db_name('f25')

w_dcod=space(3)
w_hcod='999'

w_djuri	= "00"
w_hjuri	= "24"

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F25'

use (d0f25) in 0 alias 'f25' order 'd25refe' &&(Perc./Ret. IB)

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif

=creatipos('V')	&& perc/ret/otros

fieldvec=''

*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws025l.spr

pop key
pop menu _msysmenu
=closall()

dele file &d0f99.c.dbf
=borratipos()

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons25l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons25l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons25l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid25l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

if used('f99c')
	sele f99c
	use
	dele file &d0f99.c.dbf
endif

sele 0
select *;
	from f25;
	where between(d25codi,w_dcod,w_hcod) ;
			AND BETWEEN(d25tdgi,w_djuri,w_hjuri) ;
	and iif(w_regi='A',.t.,d25refe=w_regi) ;
	into CURSOR f99c 

	INDEX ON d25tdgi + d25refe + d25cafi + d25codi		TO f99b

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
go top
if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr025l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

USE 

DELE FILE f99b.idx

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid25l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

IF w_vari='W_DJURI' .or. w_show
		w_djuri	= STRCERO(w_djuri)
		IF !EMPTY(w_djuri) or lastkey()=p_f4
				DO fhelp WITH 'W_DJURI','JD','iwp001t','f01t','clav','desc','Jurisdicci๓n DGI Inexistente',(p_strc),'w_retu'
		ENDIF
		SHOW GET w_djuri
ENDIF

IF w_vari='W_HJURI' .or. w_show
		w_hjuri	= STRCERO(w_hjuri)
		IF !EMPTY(w_hjuri) or lastkey()=p_f4
				DO fhelp WITH 'W_HJURI','JD','iwp001t','f01t','clav','desc','Jurisdicci๓n DGI Inexistente',(p_strc),'w_retu'
		ENDIF
		SHOW GET w_hjuri
ENDIF

if w_vari='W_DCOD' or w_show
	w_dcod=strcero(w_dcod)
	show get w_dcod
	if lastk()=p_f4
		do fhelp with 'W_DCOD','','iwp025','f25','d25refe','d25codi','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_REGI' or w_show
	if !w_regi$'PRA'
		w_retu=.f.
	endif
endif

if w_vari='W_HCOD' or w_show
	w_hcod=strcero(w_hcod)
	show get w_hcod
	if lastk()=p_f4
		do fhelp with 'W_HCOD','','iwp025','f25','d25refe','d25codi','',(p_strc),'w_retu'
	endif
	if w_dcod>w_hcod
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr025l
