
%{
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

extern FILE* yyin;
extern char* yytext;
extern int yylineno;

 typedef struct symbol {
char  name[10], type[10], value[100], scope[10], fct_sgn[100] , element[20], poz[5];
}simbol;

simbol simb, table[100];
int i =0;

void insert(simbol pr);
void verifyIdVariable(simbol s);
void verifyFunDecl(simbol s);
void verifyIdFun(simbol s);
void exists(simbol s);
void verifyIdVector(simbol s);

void makefile();
void assignPozVect(simbol s);
void verifyStruct(simbol s);
void verifyIdStruct (simbol s);

%}
%token ID TIP BGIN END ASSIGN NR NRR STRUCT CHR BOOL IF THEN ELSE FOR PLUS MINUS TIMES DIVIDE MODULO STRLEN LT GT EQ OR AND DIF NOT lpar rpar EVAL STRCAT STRCPY STRUCTID
%start S
%left '+'
%left '*'
%%

S: declaratii bloc S
  | declaratii bloc
        ;

declaratii: declaratie
          | declaratii declaratie
          ;

/*
id-assign-cst => verifyidvariable
tip-id => exists
vector-assign nr =>
verifyStruct
assignPozVect

*/

declaratie : TIP ID ';' {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"variable"); strcpy(simb.poz,"-"); exists(simb);}
        | TIP ID lpar NR rpar ';'               {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, (char*)$4);
                                                                        strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"array"); strcpy(simb.poz,"-"); verifyIdVector(simb);}
        |  TIP ID '(' param_list ')'    {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, '-');
                                                                                strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,(char*) $3); strcpy(simb.element,"function"); strcpy(simb.poz,"-"); verifyFunDecl(simb);}
        | TIP ID '('  ')'                       {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                                        strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"no param"); strcpy(simb.element,"function"); strcpy(simb.poz,"-"); verifyFunDecl(simb);}
        | STRUCT STRUCTID '{' param_struct '}' ';'   {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,"struct"); strcpy(simb.value, (char*)$3);
                                                                                                strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"struct");strcpy(simb.poz,"-"); verifyStruct(simb);}
        | TIP ID ASSIGN cst ';'                 {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, (char*)$4);
                                                 strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"variable"); strcpy(simb.poz,"-"); exists(simb);}

        | ID ASSIGN cst ';'                     {strcpy(simb.name, (char*)$1) ;  strcpy(simb.value, (char*) $3); strcpy(simb.element, "variable"); verifyIdVariable(simb);}
        | ID lpar NR rpar ASSIGN cst ';'                        {strcpy(simb.name, (char*)$1) ; strcpy(simb.poz, (char*) $3); strcpy(simb.value, (char*) $6); assignPozVect(simb);}
        | EVAL '(' AExp')' ';'
                | STRUCTID ID ';' {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                strcpy(simb.scope,"global"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"struct"); strcpy(simb.poz,"-"); verifyIdStruct(simb);}
                 ;
param_list : paramL     //{scop)
        | param_list ',' paramL
        ;
paramL :  TIP ID                {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                strcpy(simb.scope,"signature"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"variable"); strcpy(simb.poz,"-"); exists(simb);}
{ strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.scope,"signature"); strcpy(simb.fct_sgn, "yes"); insert(simb);}
        | TIP ID lpar NR rpar   { strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1);strcpy(simb.value, (char*) $4); strcpy(simb.scope,"signature"); strcpy(simb.fct_sgn, "yes"); verifyIdVector;}
        ;
param_struct : paramS //{scop}
            |param_struct ',' paramS
            ;
paramS :  TIP ID                        { strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.scope,"sruct"); insert(simb);}
        | TIP ID lpar NR rpar  { strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, (char*) $4); strcpy(simb.scope,"struct"); verifyIdVector(simb);}
        ;



bloc :  BGIN list END
        | BGIN list bloc list END
        | BGIN list bloc END
        | BGIN bloc list END
        ;

list : statement ';'
        | list statement ';'
        ;

statement: TIP ID                       {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                        strcpy(simb.scope,"local"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"variable"); strcpy(simb.poz,"-"); exists(simb);}
        | TIP ID lpar NR rpar           {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value,(char*)$4);
                                                            strcpy(simb.scope,"local"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"vector"); strcpy(simb.poz,"-"); verifyIdVector(simb);}
        | TIP ID ASSIGN cst {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, (char*)$4);
                                                        strcpy(simb.scope,"local"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"variable"); strcpy(simb.poz,"-"); exists(simb);}
                | STRUCT STRUCTID '{' param_struct '}' ';'   {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,"struct"); strcpy(simb.value, (char*)$3);
                                                                                                strcpy(simb.scope,"local"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"struct");strcpy(simb.poz,"-"); verifyStruct(simb);}

        | ID ASSIGN cst                  {strcpy(simb.name, (char*)$1) ;  strcpy(simb.value, (char*) $3); strcpy(simb.element, "variable"); verifyIdVariable(simb);}
        | ID lpar NR rpar ASSIGN cst    {strcpy(simb.name, (char*)$1) ;  strcpy(simb.poz, (char*) $3); strcpy(simb.value, (char*) $6); assignPozVect(simb);}
        | ID '(' lista_apel ')'         {strcpy(simb.name, (char*)$1) ;  strcpy(simb.value, (char*) $2); verifyIdFun(simb);}
        | EVAL '(' AExp ')'
        | FOR '(' ID ASSIGN NR '~' BExp '~' statement ')' '{' list '}'
        | 'while' '(' BExp ')' '{' list '}'
        | IF '(' BExp ')' THEN '{' list '}' ELSE '{' list '}'
        | IF '(' BExp ')' THEN '{' list '}'
        | ID STRCPY ID
        | ID STRCAT ID
                | STRUCTID ID {strcpy(simb.name, (char*)$2) ; strcpy(simb.type,(char*)$1); strcpy(simb.value, "-");
                                                strcpy(simb.scope,"local"); strcpy(simb.fct_sgn,"-"); strcpy(simb.element,"struct"); strcpy(simb.poz,"-"); verifyIdStruct(simb);}
        ;

lista_apel: cst
        | cst ',' lista_apel
        ;

cst :  AExp
        | CHR
        | BExp
        ;


AExp:  NR
    | NRR
    | ID
    | STRLEN '(' ID ')'
    | PLUS '(' AExp ',' AExp ')'
    | MINUS  '(' AExp ',' AExp ')'
    | TIMES '(' AExp ',' AExp ')'
    | DIVIDE '(' AExp ',' AExp ')'
    | MODULO '(' AExp ',' AExp ')'
    ;

BExp : BOOL
    | LT  '(' AExp ',' AExp ')'
    | GT '(' AExp ',' AExp ')'
    | EQ '(' AExp ',' AExp ')'
    | DIF '(' AExp ',' AExp ')'
    | AND '(' BExp ',' BExp ')'
    | OR '(' BExp ',' BExp ')'
    | NOT BExp
    ;


%%


/*functie ce verifica daca pt o regula de tipul ID ASSIGN cst
  id-ul este existent(cu acelasi tip si element)
  si, in caz afirmativ, modifica valoarea
 */
void verifyIdVariable(simbol s)
{
        int j;
                bool ok =false;
        for(j=0; j<i; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].element, s.element)==0)
                {
                                                ok= true;
                        strcpy(table[j].value, s.value);
                                                makefile();
                         break;
                }
        }
                if(ok==false)
                {
                        printf("Eroare! S-a incercat atribuirea variabilei nedeclarate %s\n", s.name);
                        exit(0);
                }
}

void verifyIdStruct (simbol s)
{
        int j;
        bool st=false;
        bool el=false;
        for(j=0; j<i;j++)
        {
                if(strcmp(table[j].element, s.element)==0 && strncmp( s.element, "struct", 6)==0 && strcmp(table[j].name, s.type)==0)
                {
                        st=true;
                                                break;
                }
        }
        for(j=0;j<i; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].element, s.element)==0 && strcmp(table[j].type, s.type)==0)
                {
                        el=true;
                        //exista elementul
                        break;
                }
        }
        if(st==true && el == false)
        {
                insert(s);
        }
        else
        {
                printf("Structura nedeclarata sau element deja existent!\n");
                exit(0);
        }
}

void makefile()
{
        FILE *fp;
        fp = fopen("symbol_table.txt", "w");
         int j;
         fprintf(fp, "NUME   ;;   TIP   ;;   VALOARE   ;;   SCOPE   ;;   SIGNATURA   ;;   TIP ELEMENT   ;;   POZITIA\n\n");

        for(j = 0; j< i; j++)
        {
                fprintf(fp,"%s  ;;  %s   ;;   %s   ;;   %s   ;;   %s   ;;   %s   ;;   %s\n", table[j].name, table[j].type, table[j].value, table[j].scope, table[j].fct_sgn,table[j].element,table[j].poz);
        }
        fclose(fp);
}


void insert (simbol pr)
{
    strcpy(table[i].name,pr.name);
    strcpy(table[i].type,pr.type);
    strcpy(table[i].value,pr.value);
    strcpy(table[i].scope,pr.scope);
    strcpy(table[i].fct_sgn,pr.fct_sgn);
    strcpy(table[i].element,pr.element);
    strcpy(table[i].poz,pr.poz);
    printf("%s\n%s\n%s\n%s\n%s\n%s\n%s\n", table[i].name, table[i].type, table[i].value, table[i].scope, table[i].fct_sgn, table[i].element, table[i].poz);

    i++;
    makefile();
}


/*
verifica daca a mai fost declarata
verifica id, tip si element
*/
void exists(simbol s)
{
        int j;
        bool ok=false;
        for(j=0; j<i && ok==false; j++)
        {
                if(strcmp(table[j].name, s.name)==0)
                {
                        if(strcmp(table[j].type, s.type)==0 && strcmp(table[j].element, s.element)==0)
                                ok=true;
                }
        }
        if(ok==false)
                insert(s);
                if(ok==true)
                {
                        printf("Eroare! S-a incercat declararea variabilei %s declarate anterior \n", s.name);
                        exit(0);
                }
}

/*
verifica daca id-ul vectorului nu e prezent in tabel
in caz negativ, adauga prima linie ca fiind cea a declaratiei de vector(s)
in caz pozitiv, populeaza tabelul cu atatea linii cate elem exista
*/
void verifyIdVector(simbol s)
{
        int j;
        bool ok=false;
        for(j=0; j<i && ok==false; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].type, s.type)==0)
                {
                        ok=true;
                }
        }
        if(ok==false)
        {
                //inseram vector
                insert(s);

                //inseram atatea cate elem are
                //convert de la string la int pt value=dimensiune
                int dimensiune;
                char try[2];
                strcpy(try, s.value);

                dimensiune=atoi(try);

                simbol aux;
                //char  name[10], type[10], value[100], scope[10], fct_sgn[5] , element[20], poz;
                strcpy(aux.name, s.name);
                strcpy(aux.type, s.type);
                strcpy(aux.value, "-");
                strcpy(aux.scope, s.scope);
                strcpy(aux.fct_sgn, s.fct_sgn);
                strcpy(aux.element, s.element);
                int j;
                for(j=0; j<dimensiune; j++)
                {
                printf("Sunt in for %d \n", j);
                        sprintf(aux.poz,"%d",j);

                        insert(aux);
                }

        }
                if(ok==true)
                {
                        printf("Eroare! S-a incercat declararea vectorului %s declarat anterior!\n", s.name);
                        exit(0);
                }
}

//verifyFuncDecl
void verifyFunDecl(simbol s)
{
        int j;
        bool ok=false;
        for(j=0; j<i && ok==false; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].element, s.element)==0 && strncmp(table[j].element, "function", 8)==0 && strcmp(table[j].type, s.type)==0)
                {
                        ok=true;
                }
        }
        if(ok==false)
        {
                insert(s);
        }
                if(ok==true)
                {
                        printf("Eroare! Functia %s este deja declarata!\n", s.name);
                        exit(0);
                }
}



void assignPozVect(simbol s)
{
        int j;
        for(j=0; j<i;j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].name, s.name)==0
                && strcmp(table[j].poz, s.poz)==0 )
                {
                        strcpy(table[j].value,s.value);
                                                makefile();
                }
        }
}
void verifyStruct(simbol s)
{
        int j;
        bool ok=false;
        for(j=0; j<i && ok==false; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].element, s.element)==0 && strncmp( s.element, "struct", 6)==0)
                {
                        ok=true;
                }
        }
        if(ok==false)
        {
                insert(s);
        }
                if(ok==true)
                {
                        printf("Eroare! Struct %s este deja declarat!\n", s.name);
                        exit(0);
                }
}
//verifyIdFun
void verifyIdFun(simbol s)
{
        int j;

        for(j=0; j<i; j++)
        {
                if(strcmp(table[j].name, s.name)==0 && strcmp(table[j].element, s.element)==0 && strncmp(table[j].element, "function", 8)==0 )
                {
                        strcpy(table[j].value, s.value);
                                                makefile();
                }
        }

}



void yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
yyin=fopen(argv[1],"r");
yyparse();
}
