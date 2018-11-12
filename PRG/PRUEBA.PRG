PARAMETERS w_pref,w_call

private all like w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear      &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

*) Variables internas----------------------------------------------------------

w_prog =upper(prompt())
w_alta =.f.
w_pfile='900i'
w_title='INCORPORACION DE COMPROBANTES'

CLOSE Data
set procedure to \ss_fox\library
use k:\iw\00\ivf000 in 0 alias 'f00'
use K:\ivf033 in 0 alias 'f33'
use K:\ivf033c in 0 alias 'f33c'
use k:\iw\00\inte001v in 0 alias 'fit'
use k:\iw\00\inte001c in 0 alias 'fic'
use k:\iw\00\ivf007 in 0 alias 'f07'
use k:\iw\00\ivf002 in 0 alias 'f02' orde d2cuit
use k:\iw\00\ivf003 in 0 alias 'f03' orde d3cuit
use k:\iw\00\ivf001 in 0 alias 'f01'
use k:\iw\ivf023 in 0 alias 'f23' orde d23desc

*////////// VENTAS \\\\\\\\\\*

SELE Fit
GO TOP IN F33
DO WHILE !EOF('f33')

	GO BOTTOM
	STORE fit.D5inte to ld5inte
	ld5inte = Val(ld5inte)+1

	append blank

*****	Alicuotas	*****

	
	GO TOP IN F23
	DO WHILE !EOF('f23')

		if f23.d23alic=f33.tasa
    	replace fit.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO
	
*****	Comp.Generico	*****

	replace fit.D5fech with CTOD(f33.fecha),;
			fit.D5empr with f07.D7empr,;
			fit.D5peri with subs(f33.fecha,4,7),;
			fit.D5inte with Padl(alltr(str(ld5inte,6)), 6,'0'),;
			fit.D5sucu with subs(f33.Desde_fact,5,8),;
			fit.D5por1 with f33.Tasa,;
			fit.D5imp1 with f33.Neto,;
			fit.D5imp4 With f33.Iva_insc,;
			fit.D5tota with f33.Total
	lCuit = f33.Cuit		
				
*****	Comp.Tique Z	*****
		
	DO CASE	
		CASE subs(F33.Desde_fact,1,3)='Tic'
			replace fit.D5tifo with 'Z',;
					fit.D5tico with '?',;
					fit.D5tipo with 'V',;
					fit.D5coco with '080',;
					fit.D5imfi with 'S',;
					fit.D5nume with subs(F33.Desde_fact,10,8)+'/'+subs(F33.Hasta_fact,14,4),;
					fit.D5clie with '99993',;
					fit.D5cond with '1',;
					fit.D5coim with '1',;
					fit.D5tiim with '3',;
					fit.D5tiop with '0'
		
*****	Facturas A	*****

		CASE subs(F33.Desde_fact,1,3)='FcA'
			replace fit.D5tifo with 'A',;
					fit.D5tico with 'A',;
					fit.D5tipo with 'V',;
					fit.D5coco with '001',;
					fit.D5imfi with 'N',;
					fit.D5nume with subs(F33.Desde_fact,10,8)+'/',;
					fit.D5cond with '1',;
					fit.D5coim with '1',;
					fit.D5tiim with '3',;
		 			fit.D5tiop with '0'
		ENDCASE	

*****	Clientes	*****

	if seek(lCuit,'f02')
		replace fit.D5clie with f02.d2clie,;
				fit.D5juri with f02.d2juri		
	endif
	
*****	Rubro	*****

	if type('f33.rubro')='U'
		replace fit.d5rubr with '000000' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fit.d5rubr with f33.rubro
	endif
	

	SKIP IN F33
ENDDO

*/////////// COMPRAS \\\\\\\\\\\\*

SELE Fic
GO TOP IN F33c
DO WHILE !EOF('f33c')

	GO BOTTOM
	STORE fic.D5inte to lcd5inte
	lcd5inte = Val(lcd5inte)+1

	append blank

*****	Alicuotas	*****

	GO TOP IN F23
	DO WHILE !EOF('f23')

		if f23.d23alic=f33c.tasa
    	replace fic.d5alic with f23.D23codi
		ENDIF
 
		SKIP IN F23
	ENDDO
	
*****	Comp.Generico	*****

	replace fic.D5fech with CTOD(f33c.fecha),;
			fic.D5empr with f07.D7empr,;
			fic.D5peri with subs(f33c.fecha,4,7),;
			fic.D5inte with Padl(alltr(str(lcd5inte,6)), 6,'0'),;
			fic.D5por1 with f33c.Tasa,;
			fic.D5imp1 with f33c.Neto,;
			fic.D5imp4 With f33c.Iva,;
			fic.D5tota with f33c.Total
	lpCuit = f33c.Cuit
	
*****	Percepcion	*****

	do case
		
		case 'ING'$f33c.nombreper1	
			do case
				case f00.p0prc1=1
					replace fic.d5imp6 with f33c.percep1
				case f00.p0prc2=1
					replace fic.d5imp7 with f33c.percep1	
				case f00.p0prc3=1
					replace fic.d5imp8 with f33c.percep1
			endcase
		
		case 'IVA'$f33c.nombreper1
					replace fic.d5imp3 with f33c.percep1
		
		case 'GAN'$f33c.nombreper1
			do case
				case f00.p0prc1=2
					replace fic.d5imp6 with f33c.percep1
				case f00.p0prc2=2
					replace fic.d5imp7 with f33c.percep1	
				case f00.p0prc3=2
					replace fic.d5imp8 with f33c.percep1
			endcase
			
		case 'OTR'$f33c.nombreper1
			do case
				case f00.p0prc1=3
					replace fic.d5imp6 with f33c.percep1
				case f00.p0prc2=3
					replace fic.d5imp7 with f33c.percep1	
				case f00.p0prc3=3
					replace fic.d5imp8 with f33c.percep1
			endcase
			
		case 'SIN'$f33c.nombreper1
			do case
				case f00.p0prc1=4
					replace fic.d5imp6 with f33c.percep1
				case f00.p0prc2=4
					replace fic.d5imp7 with f33c.percep1	
				case f00.p0prc3=4
					replace fic.d5imp8 with f33c.percep1
			endcase
	endcase
	
	if type('f33c.Tipo_percep_iva')='U'
		replace fic.d5pere with '000' 		&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5pere with f33c.Tipo_percep_iva
	endif
			
*****	Facturas A	*****

	DO CASE
		CASE subs(F33c.comprobant,1,3)='FcA'
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with '001',;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(F33c.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with '1',;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'
*****	NC A *****		
	
		CASE subs(F33c.comprobant,1,3)='NC '
			replace fic.D5tifo with 'A',;
					fic.D5tico with 'A',;
					fic.D5tipo with 'C',;
					fic.D5coco with '001',;
					fic.D5sucu with '',;
					fic.D5imfi with 'N',;
					fic.D5nume with subs(F33c.comprobant,5,13),;
					fic.D5cond with '1',;
					fic.D5coim with '1',;
					fic.D5tiim with '3',;
		 			fic.D5tiop with '0'

	ENDCASE	

*****	Proveedores	*****

	if seek(lpCuit,'f03')
		replace fic.D5prov with f03.d3prov,;
				fic.D5juri with f03.d3juri		
	endif
	
	

*****	Rubro	*****

	if type('f33c.rubro')='U'
		replace fic.d5rubr with '000000' 	&& codigo arbitrario para interfase, debe existir en la empresa
	else
		replace fic.d5rubr with f33c.rubro
	endif
		

	SKIP IN F33c
ENDDO


RETURN
