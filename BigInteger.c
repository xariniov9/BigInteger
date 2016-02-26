#include <stdio.h>
#include<string.h>

#define	MAXBigNumD	500		 
#define PLUS		1		
#define MINUS		-1		
#define BASE		10

typedef struct {
        char BigNumD[MAXBigNumD];         
	int parity;			 
        int length;			
}BigInteger;

char tmp1[MAXBigNumD], tmp2[MAXBigNumD];


Align(BigInteger *n){
	while ((n->length > 0) && (n->BigNumD[ n->length ] == 0))
		n->length--;
        if ((n->length == 0) && (n->BigNumD[0] == 0))
		n->parity = PLUS;
}

int 
min(int a, int b){
	return a<b?a:b;
}

int 
max(int a, int b){
	return a>b?a:b;
}

void
print(BigInteger *n){
	int i;
	if(n->parity==-1)	printf("-");
	for (i=n->length; i>=0; i--)
		printf("%c",'0'+ n->BigNumD[i]);
	printf("\n");
}

void
initialize_BigInteger(BigInteger *n){
	int i;
	for(i=0; i<=MAXBigNumD; i++)
		n->BigNumD[i]=(char) 0;
	n->parity=1;
	n->length=-1;
}

init(BigInteger *n, char *s){
	int i, t, flag=0;	
	if(s[0]=='-'){
		flag=1;
		n->parity=-1;
	}

	else	
		n->parity = 1;
	for(i=0; i<MAXBigNumD; i++) 
		n->BigNumD[i] = (char) 0;
	n->length = -1;
	i=strlen(s)-1;
	while(i>=flag){
		n->length++;
		n->BigNumD[n->length]= s[i]-'0';
		i--;
	}
	Align(n);
}

Multiply(BigInteger *a, BigInteger *b, BigInteger *c){
	BigInteger row;			
	BigInteger tmp;			
	int i,j;			
	initialize_BigInteger(c);
	row = *a;
	for (i=0; i<=b->length; i++) {
		for (j=1; j<=b->BigNumD[i]; j++) {
			Add(c,&row,&tmp);
			*c = tmp;
		}
		Shft(&row,1);
	}
	c->parity = a->parity * b->parity;
	Align(c);
}

Add(BigInteger *a, BigInteger *b, BigInteger *c){
	int carry, i;		
	initialize_BigInteger(c);
	if (a->parity == b->parity) c->parity = a->parity;
	else {
		if (a->parity == MINUS) {
			a->parity = PLUS;
			Minus(b,a,c);
			a->parity = MINUS;
		} else {
                        b->parity = PLUS;
                        Minus(a,b,c);
                        b->parity = MINUS;
		}
		return;
	}
	c->length = max(a->length,b->length)+1;
	carry = 0;
	for (i=0; i<=(c->length); i++) {
		c->BigNumD[i] = (char) (carry+a->BigNumD[i]+b->BigNumD[i]) % BASE;
		carry = (carry + a->BigNumD[i] + b->BigNumD[i]) / BASE;
	}
	Align(c);
}


Minus(BigInteger *a, BigInteger *b, BigInteger *c){
	int borrow, v, i;		
	initialize_BigInteger(c);
	if ((a->parity == MINUS) || (b->parity == MINUS)) {
                b->parity = -1 * b->parity;
                Add(a,b,c);
                b->parity = -1 * b->parity;
		return;
        }
	if (Cmp(a,b) == PLUS) {
		Minus(b,a,c);
		c->parity = MINUS;
		return;
	}
        c->length = max(a->length,b->length);
        borrow = 0;
        for (i=0; i<=(c->length); i++) {
		v = (a->BigNumD[i] - borrow - b->BigNumD[i]);
		if (a->BigNumD[i] > 0)
			borrow = 0;
		if (v < 0) {
			v = v + BASE;
			borrow = 1;
		}
                c->BigNumD[i] = (char) v % BASE;
        }
	Align(c);
}

Cmp(BigInteger *a, BigInteger *b){
	int i;		
	if ((a->parity == MINUS) && (b->parity == PLUS)) return(PLUS);
	if ((a->parity == PLUS) && (b->parity == MINUS)) return(MINUS);
	if (b->length > a->length) return (PLUS * a->parity);
	if (a->length > b->length) return (MINUS * a->parity);
	for (i = a->length; i>=0; i--) {
		if (a->BigNumD[i] > b->BigNumD[i]) return(MINUS * a->parity);
		if (b->BigNumD[i] > a->BigNumD[i]) return(PLUS * a->parity);
	}
	return(0);
}

Shft(BigInteger *n, int d){
	int i;			
	if ((n->length == 0) && (n->BigNumD[0] == 0)) return;
	for (i=n->length; i>=0; i--)
		n->BigNumD[i+d] = n->BigNumD[i];
	for (i=0; i<d; i++) n->BigNumD[i] = 0;
	n->length = n->length + d;
}

Division(BigInteger *a, BigInteger *b, BigInteger *c){
        BigInteger row;                     
        BigInteger tmp;                     
	int asign, bsign;		
        int i,j;                        
	initialize_BigInteger(c);
	c->parity = a->parity * b->parity;
	asign = a->parity;
	bsign = b->parity;
	a->parity = PLUS;
        b->parity = PLUS;
	initialize_BigInteger(&row);
	initialize_BigInteger(&tmp);
	c->length = a->length;
	for (i=a->length; i>=0; i--) {
		Shft(&row,1);
		row.BigNumD[0] = a->BigNumD[i];
		c->BigNumD[i] = 0;
		while (Cmp(&row,b) != PLUS) {
			c->BigNumD[i] ++;
			Minus(&row,b,&tmp);
			row = tmp;
		}
	}
	Align(c);
	a->parity = asign;
	b->parity = bsign;
}

int
main(){
	int t;
	char str[2]="1";
	BigInteger n1,n2,n3,L;
	scanf("%d",&t);
	while (t--){
		scanf("%s%s",tmp1,tmp2);
		init(&n1, tmp1); 
		init(&n2, tmp2);
		Division(&n1,&n2,&n3);
		print(&n3);
		Minus(&n3, &n2, &n1);
		print(&n1);
	}
	return 0;
}
