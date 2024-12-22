#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <conio.h>

typedef struct {
    char type;
    struct Sign *next;
} Sign;//isaretleri saklayacagimiz linked list

Sign *headsign;

typedef enum {
    SIN,
    COS,
    TAN,
    COT
} TRI_TYPE;

typedef enum {
    ARCSIN,
    ARCCOS,
    ARCTAN,
    ARCCOT
} ADTRI_TYPE;

typedef struct {
	float coef;
	float exp;
	float control;
}POLI;//polinom

typedef struct {
	float coef;
	float exp;
	float fcoef;
	float fexp;
	float base;
	float control;
}UST;//ustel fonksiyon

typedef struct {
	float coef;
	float exp;
	float fcoef;
	float fexp;
	float base;
	float control;
}LOG;//logaritma

typedef struct {
	float coef;
	float exp;
	float fcoef;
	float fexp;
	float control;
	TRI_TYPE trig;
}TRI;//trigonometrik fonksiyon

typedef struct {
	float coef;
	float exp;
	float fcoef;
	float fexp;
	float control;
	ADTRI_TYPE trig;
}ADTRI;//ters trigonometrik fonksiyonlar

typedef struct {
	POLI polinom;
	UST ustel;
	LOG logaritma;
	TRI trigono;
	ADTRI terstrigono;
	float constant;
    struct TERIM *next;
}TERIM;//genel terim linked list

TERIM *head;

typedef struct {
	float xcoef;
	float ycoef;
	float zcoef;
	float result;
}Seidel;//gauss seidel icin

TERIM * CreateTERIM ();
Sign * CreateSign ();
void AddTERIM (TERIM *x);
void AddSign (Sign *x);
void parse ();
float fx (float x);
float turev (float x);
float determinant(float **matrix, int N);
void swapRows(float **matrix, int row1, int row2, int size);
void scaleRow(float **matrix, int row, float scale, int size);
void addRows(float **matrix, int row1, int row2, float scale, int size);
float** inverse(float **matrix, int size);
float f1(float x, float y, float z, Seidel m);
float f2(float x, float y, float z, Seidel m);
float f3(float x, float y, float z, Seidel m);
void ilerifark ();
void gerifark ();
void ortanokta ();
void ikinciturev ();
void simpson1_3 ();
void simpson3_8 ();
void Bisection ();
void RegulaFalsi ();
void NewtonRaphson ();
void InverseMatrix ();
void GaussElemination ();
void GaussSeidel ();
void NumericalDifferentiation ();
void SimpsonsRule ();
void TrapezoidalRule ();
void GregoryNewton ();

int main() {
	setlocale(LC_ALL, "Turkish");
	
	int X=1;
	float value;
	float **matrix;
	
    while(X != 0){
    	printf("\n\nQuit : 0\n");
    	printf("Bisection : 1\n");
    	printf("Regula Falsi : 2\n");
    	printf("Newton Raphson : 3\n");
    	printf("Inverse Matrix : 4\n");
    	printf("Gauss Elemination : 5\n");
    	printf("Gauss-Seidel : 6\n");
    	printf("Numerical Differentiation : 7\n");
    	printf("Simpson's Rule : 8\n");
    	printf("Trapezoidal Rule : 9\n");
    	printf("Gregory-Newton : 10\n");
    	printf("Choice : ");
    	scanf(" %d",&X);
    
		if(X == 1){
			Bisection();
		} else if(X == 2){
    		RegulaFalsi();
		} else if(X == 3){
			NewtonRaphson();
		} else if(X == 4){
			InverseMatrix();
		} else if(X == 5){
			GaussElemination();
		} else if(X == 6){
			GaussSeidel();
		} else if(X == 7){
			NumericalDifferentiation();
		} else if(X == 8){
			SimpsonsRule();
		} else if(X == 9){
			TrapezoidalRule();
		} else if(X == 10){
			GregoryNewton();
		} else 
		printf("\ninvalid input please enter a valid input\n");
	}
    return 0;  
}
void parse() {
    int i=0, j, n, m, y, k, l;
    char expr[100];
    float coef, pow, fcoef, fpow, base;
    const char *trigono[4] = {"sin", "cos", "tan", "cot"};
	fflush(stdin);
    printf("Fonksiyonu girin: ");
    fgets (expr, sizeof(expr), stdin);
    expr[strcspn(expr, "\n")] = '\0';
    
    while(expr[i] != '\0'){
    	if (expr[i] == '+') {
    		Sign *t = CreateSign();
			t->type	= '+';
    		AddSign(t);
		} else if (expr[i] == '-') {
			Sign *t = CreateSign();
			t->type	= '-';
    		AddSign(t);
		} else if (expr[i] == 'X') {
			Sign *t = CreateSign();
			t->type	= 'X';
    		AddSign(t);
		} else if (expr[i] == '/') {
			Sign *t = CreateSign();
			t->type	= '/';
    		AddSign(t);
		}
		i++;
	}//terimler arasi isaretleri linked listte sakliyoruz

    char *token = strtok(expr, "+-X/");
    while (token != NULL) {
    	int flag = 0;
        TERIM *x = CreateTERIM();
        char *result = strstr(token, "x");
        if (result == NULL) {
            n = 1;
        } else {
            n = 0;
        }
        result = strstr(token, "arc");
        if (result == NULL) {
            m = 1;
        } else {
            m = 0;
        }
        for (i = 0; i < 4; i++) {
            result = strstr(token, trigono[i]);
            if (result == NULL || m == 0) {
                y = 1;
            } else {
                y = 0;
                i = 5;
            }
        }
        result = strstr(token, "l");
        if (result == NULL) {
            k = 1;
        } else {
            k = 0;
        }
        result = strstr(token, "^(");
        if (result == NULL) {
            l = 1;
        } else {
            l = 0;
        }
        if (l == 0 && y == 1 && m == 1 && k == 1) {
            x->ustel = *(UST*) malloc (sizeof(UST));
            char *ptr = token;
            if (*ptr == ' ')
			ptr++;
            if (isdigit(*ptr) || *ptr == '.') {
                fcoef = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            } else {
                fcoef = 1.0;
                flag = 1;
            }
            ptr = strstr(token, "*");
            if (ptr != NULL){
        		ptr++;
            	if (*ptr == '(' && flag == 0) {
                	ptr++;
                	if (*ptr == 'e')
                    	base = M_E;
                	else if (isdigit(*ptr) || *ptr == '.') {
                    	base = atof(ptr);
                    	while (isdigit(*ptr) || *ptr == '.') ptr++;
                	}
           		} else if (flag == 1) {
                	if (*ptr == 'e')
                    	base = M_E;
                	else if (isdigit(*ptr) || *ptr == '.') {
                    	base = atof(ptr);
                    	while (isdigit(*ptr) || *ptr == '.') ptr++;
                	}
            	}
			} else {
				ptr = strstr(token, "^");
				if (ptr != NULL){
					ptr--;
					if (*ptr == 'e')
                    	base = M_E;
                	else if (isdigit(*ptr) || *ptr == '.') {
                    	base = atof(ptr);
                    	while (isdigit(*ptr) || *ptr == '.') ptr++;
                	}
				}
			} 
			
            ptr = strstr(token, "^(");
            if (ptr != NULL){
        		ptr += 2;
            	if (isdigit(*ptr) || *ptr == '.') {
                	coef = atof(ptr);
                	while (isdigit(*ptr) || *ptr == '.') ptr++;
            	} else {
                	coef = 1.0;
            	}
			} else {
				coef = 1.0;
			}
			
            ptr = strstr(token, "x");
            if (ptr != NULL){
        		ptr++;
            	if (*ptr == '^') {
                	ptr++;
                	if (isdigit(*ptr) || *ptr == '.') {
                    	pow = atof(ptr);
                    	while (isdigit(*ptr) || *ptr == '.') ptr++;
                	}
            	} else {
                	pow = 1.0;
            	}
			} else {
				pow = 0;
			}
			
            ptr = strstr(token, ")^");
            if (ptr != NULL){
        		ptr += 2;
            	if (isdigit(*ptr) || *ptr == '.') {
                	fpow = atof(ptr);
                	while (isdigit(*ptr) || *ptr == '.') ptr++;
            	} else {
                	fpow = 1.0;
            	}
			} else {
				fpow = 1.0;
			}
			
            x->ustel.base = base;
            x->ustel.coef = coef;
            x->ustel.exp = pow;
            x->ustel.fcoef = fcoef;
            x->ustel.fexp = fpow;
            x->ustel.control = 1.0;
        } else if (k == 0) {
            x->logaritma = *(LOG*) malloc (sizeof(LOG));
            char *ptr = token;
            if (*ptr == ' ')
			ptr++;
            if (isdigit(*ptr) || *ptr == '.') {
                fcoef = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            } else {
                fcoef = 1.0;
            }
            ptr = strstr(token, "_");
            if (ptr != NULL) {
            	ptr++;
            	if (isdigit(*ptr) || *ptr == '.') {
                base = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            	} else if (*ptr == 'e') {
            		base = M_E;
				}
			}
			ptr = strstr(token, "ln");
			if (ptr != NULL) {
    			base = M_E;
			}
			ptr = strstr(token, "_");
			if (ptr != NULL){
        		ptr += 2;
				if (*ptr == '(') {
            		ptr++;
            		if (isdigit(*ptr) || *ptr == '.') {
                		coef = atof(ptr);
                		while (isdigit(*ptr) || *ptr == '.') ptr++;
            		} else {
                		coef = 1.0;
            		}
				} 
			} else {
				ptr = strstr(token, "ln");
				if (ptr != NULL){
        			ptr += 2;
					if (*ptr == '(') {
            			ptr++;
            			if (isdigit(*ptr) || *ptr == '.') {
                			coef = atof(ptr);
                			while (isdigit(*ptr) || *ptr == '.') ptr++;
                		}
            		} else {
                		coef = 1.0;
            		}
				} else {
					coef = 1.0;
				}
            }	
            ptr = strstr(token, "x");
            if (ptr != NULL){
        		ptr++;
            	if (*ptr == '^') {
                	ptr++;
                	if (isdigit(*ptr) || *ptr == '.') {
                    	pow = atof(ptr);
                    	while (isdigit(*ptr) || *ptr == '.') ptr++;
                	}
            	} else {
                	pow = 1.0;
            	}
			} else {
				pow = 0;
			}
			
            ptr = strstr(token, ")^");
			if (ptr != NULL){
				ptr += 2;
				if (isdigit(*ptr) || *ptr == '.') {
                fpow = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            	} else {
                	fpow = 1.0;
            	}
			} else {
				fpow = 1.0;
			}
            
            x->logaritma.base = base;
            x->logaritma.coef = coef;
            x->logaritma.exp = pow;
            x->logaritma.fcoef = fcoef;
            x->logaritma.fexp = fpow;
            x->logaritma.control = 1.0;
        } else if ( y == 0 ) {
        	x->trigono = *(TRI*) malloc (sizeof(TRI));
        	char *ptr = token;
        	if (*ptr == ' ')
			ptr++;
            if (isdigit(*ptr) || *ptr == '.') {
                fcoef = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            } else {
                fcoef = 1.0;
            }
            for (i = 0; i < 4; i++) {
            	ptr = strstr(token, trigono[i]);
            	if (ptr != NULL){
            		i = 5;
				}
        	}
        	if (*ptr == 's'){
        		x->trigono.trig = SIN;
			} else if (*ptr == 't'){
        		x->trigono.trig = TAN;
			} else if (*ptr == 'c'){
				ptr += 2;
				if (*ptr == 's') {
					x->trigono.trig = COS;
				} else {
					x->trigono.trig = COT;
				}
			}
			ptr = strstr(token, "(");
			if (ptr != NULL){
        		ptr++;
				if (isdigit(*ptr) || *ptr == '.') {
            		coef = atof(ptr);
            		while (isdigit(*ptr) || *ptr == '.') ptr++;
        		} else {
            		coef = 1.0;
        		}
			} else {
				coef = 1.0;
			}
			
        	ptr = strstr(token, "x");
        	if (ptr != NULL){
        		ptr++;
        		if (*ptr == '^') {
            		ptr++;
            		if (isdigit(*ptr) || *ptr == '.') {
                		pow = atof(ptr);
                		while (isdigit(*ptr) || *ptr == '.') ptr++;
            		}
        		} else {
            		pow = 1.0;
        		}
			} else {
				pow = 0;
			}
			
        	ptr = strstr(token, ")^");
        	if (ptr != NULL){
        		ptr += 2;
        		if (isdigit(*ptr) || *ptr == '.') {
            		fpow = atof(ptr);
            		while (isdigit(*ptr) || *ptr == '.') ptr++;
        		} else {
            		fpow = 1.0;
        		}
			} else {
				fpow = 1.0;
			}
		
        	x->trigono.coef = coef;
        	x->trigono.exp = pow;
        	x->trigono.fcoef = fcoef;
        	x->trigono.fexp = fpow;
        	x->trigono.control = 1.0;
		} else if ( m == 0 ) {
			x->terstrigono = *(ADTRI*) malloc (sizeof(ADTRI));
        	char *ptr = token;
        	if (*ptr == ' ')
			ptr++;
        	if (isdigit(*ptr) || *ptr == '.') {
            	fcoef = atof(ptr);
            	while (isdigit(*ptr) || *ptr == '.') ptr++;
        	} else {
            	fcoef = 1.0;
        	}
        	for (i = 0; i < 4; i++) {
            	ptr = strstr(token, trigono[i]);
            	if (ptr != NULL){
            		i = 5;
				}
        	}
        	if (*ptr == 's'){
        		x->terstrigono.trig = ARCSIN;
			} else if (*ptr == 't'){
        		x->terstrigono.trig = ARCTAN;
			} else if (*ptr == 'c'){
				ptr += 2;
				if (*ptr == 's') {
					x->terstrigono.trig = ARCCOS;
				} else {
					x->terstrigono.trig = ARCCOT;
				}
			}
			ptr = strstr(token, "(");
			if (ptr != NULL){
        		ptr++;
				if (isdigit(*ptr) || *ptr == '.') {
            		coef = atof(ptr);
            		while (isdigit(*ptr) || *ptr == '.') ptr++;
        		} else {
            		coef = 1.0;
        		}
			} else {
				coef = 1.0;
			}
			
        	ptr = strstr(token, "x");
        	if (ptr != NULL){
        		ptr++;
        		if (*ptr == '^') {
            		ptr++;
            		if (isdigit(*ptr) || *ptr == '.') {
                		pow = atof(ptr);
                		while (isdigit(*ptr) || *ptr == '.') ptr++;
            		}
        		} else {
            		pow = 1.0;
        		}
			} else {
				pow = 0;
			}
			
        	ptr = strstr(token, ")^");
        	if (ptr != NULL){
        		ptr += 2;
        		if (isdigit(*ptr) || *ptr == '.') {
            		fpow = atof(ptr);
            		while (isdigit(*ptr) || *ptr == '.') ptr++;
        		} else {
            		fpow = 1.0;
        		}
			} else {
				fpow = 1.0;
			}
        	x->terstrigono.coef = coef;
        	x->terstrigono.exp = pow;
        	x->terstrigono.fcoef = fcoef;
        	x->terstrigono.fexp = fpow;
        	x->terstrigono.control = 1.0;
		} else if (l == 1 && y == 1 && m == 1 && k == 1 && n == 0) {
			x->polinom = *(POLI*) malloc (sizeof(POLI));
			char *ptr = token;
			if (*ptr == ' ')
			ptr++;
            if (isdigit(*ptr) || *ptr == '.') {
                coef = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            } else {
                coef = 1.0;
            }
            ptr = (char *)strstr(token, "^");
            if (ptr != NULL) {
            	ptr++;
            	if (isdigit(*ptr) || *ptr == '.') {
                	pow = atof(ptr);
                	while (isdigit(*ptr) || *ptr == '.') ptr++;
            	} else {
                	pow = 1.0;
            	}
			} else {
				pow = 1.0;
			}
            x->polinom.coef = coef;
            x->polinom.exp = pow;
            x->polinom.control = 1.0;
		} else if (l == 1 && y == 1 && m == 1 && k == 1 && n == 1) {
			char *ptr = token;
			if (*ptr == ' ') ptr++;
			if (isdigit(*ptr) || *ptr == '.') {
                x->constant = atof(ptr);
                while (isdigit(*ptr) || *ptr == '.') ptr++;
            }
		}
		AddTERIM(x);
		token = strtok(NULL, "+-");
	}
}

TERIM * CreateTERIM() {
    TERIM *x = (TERIM*)malloc(sizeof(TERIM));
    if (x == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    x->next = NULL; // Initialize next to NULL
    x->logaritma.control = 0;
    x->polinom.control = 0;
    x->terstrigono.control = 0;
    x->trigono.control = 0;
    x->ustel.control = 0;
    return x;
}

void AddTERIM(TERIM *x) {
    if (head == NULL) {
        head = x;
        return;
    }

    TERIM *p;
    for (p = head; p->next != NULL; p = p->next);

    p->next = x;
}

Sign *CreateSign() {
    Sign *x = (Sign *)malloc(sizeof(Sign));
    if (x == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    x->next = NULL; // Initialize next to NULL
    return x;
}

void AddSign(Sign *x) {
    if (headsign == NULL) {
        headsign = x;
        return;
    }

    Sign *p;
    for (p=headsign; p->next != NULL; p=p->next);

    p->next = x;
}

float fx(float x){
	TERIM *term;
	Sign *sign;
	float value,result=0;
	float coef,fcoef,exp,fexp,base;
	int flag=0;
	for (term = head, sign = headsign; term != NULL; term = term->next){
		if( term->logaritma.control != 0 ) {
			coef = term->logaritma.coef;
			fcoef = term->logaritma.fcoef;
			exp = term->logaritma.exp;
			fexp = term->logaritma.fexp;
			base = term->logaritma.base;
			value = fcoef * pow(log(coef * pow(x,exp))/log(base),fexp);
		} else if( term->polinom.control != 0 ) {
			coef = term->polinom.coef;
			exp = term->polinom.exp;
			value = coef * pow(x,exp);
		} else if( term->terstrigono.control != 0 ) {
			coef = term->terstrigono.coef;
			fcoef = term->terstrigono.fcoef;
			exp = term->terstrigono.exp;
			fexp = term->terstrigono.fexp;
			if (term->terstrigono.trig == ARCSIN){
				value = fcoef * pow(asin(coef * pow(x,exp)),fexp);
			} else if (term->terstrigono.trig == ARCCOS) {
				value = fcoef * pow(acos(coef * pow(x,exp)),fexp);
			} else if (term->terstrigono.trig == ARCTAN) {
				value = fcoef * pow(atan(coef * pow(x,exp)),fexp);
			} else if (term->terstrigono.trig == ARCCOT) {
				value = fcoef * pow(atan(1/(coef * pow(x,exp))),fexp);
			}
		} else if( term->trigono.control != 0 ) {
			coef = term->trigono.coef;
			fcoef = term->trigono.fcoef;
			exp = term->trigono.exp;
			fexp = term->trigono.fexp;
			if (term->trigono.trig == SIN){
				value = fcoef * pow(sin(coef * pow(x,exp)),fexp);
			} else if (term->trigono.trig == COS) {
				value = fcoef * pow(cos(coef * pow(x,exp)),fexp);
			} else if (term->trigono.trig == TAN) {
				value = fcoef * pow(tan(coef * pow(x,exp)),fexp);
			} else if (term->trigono.trig == COT) {
				value = fcoef * pow(1/tan(coef * pow(x,exp)),fexp);
			}
		} else if( term->ustel.control != 0 ) {
			coef = term->ustel.coef;
			fcoef = term->ustel.fcoef;
			exp = term->ustel.exp;
			fexp = term->ustel.fexp;
			base = term->ustel.base;
			value = fcoef * pow(pow(base,coef*pow(x,exp)),fexp);
		} else {
			value = term->constant;
		}
		if( flag == 1 && sign != NULL ){
			if ( sign->type == '+'){
				result += value;
			} else if ( sign->type == '-'){
				result -= value;
			} else if ( sign->type == 'X'){
				result *= value;
			} else if ( sign->type == '/'){
				result /= value;
			}
			sign = sign->next;
		} else if( flag == 0 ){
			result += value;
		}
		flag = 1;
	}
	return result;
}

float turev(float x){
	float turev;
    turev = (fx(x+0.001)-fx(x))/(0.001);
    return turev;
} 

float determinant(float **matrix, int N) {
    float s = 1, det = 0;
    int i, j, m, n, c;
    if (N == 1) {
        return matrix[0][0];
    } else {
        float **m_minor = (float **)malloc((N - 1) * sizeof(float *));
        for (i = 0; i < N - 1; i++) {
            m_minor[i] = (float *)malloc((N - 1) * sizeof(float));
        }
        det = 0;
        for (c = 0; c < N; c++) {
            m = 0;
            n = 0;
            for (i = 1; i < N; i++) {
                for (j = 0; j < N; j++) {
                    if (j != c) {
                        m_minor[m][n++] = matrix[i][j];
                        if (n == N - 1) {
                            m++;
                            n = 0;
                        }
                    }
                }
            }
            det += s * matrix[0][c] * determinant(m_minor, N - 1);
            s = -s;
        }
        for (i = 0; i < N - 1; i++) {
            free(m_minor[i]);
        }
        free(m_minor);
    }
    return det;
}
void swapRows(float **matrix, int row1, int row2, int size) {
	int i;
    for (i = 0; i < size; i++) {
        float temp = matrix[row1][i];
        matrix[row1][i] = matrix[row2][i];
        matrix[row2][i] = temp;
    }
}

void scaleRow(float **matrix, int row, float scale, int size) {
	int i;
    for (i = 0; i < size; i++) {
        matrix[row][i] *= scale;
    }
}

void addRows(float **matrix, int row1, int row2, float scale, int size) {
	int i;
    for (i = 0; i < size; i++) {
        matrix[row1][i] += scale * matrix[row2][i];
    }
}
float** inverse(float **matrix, int size) {
	int i, j;
    float **augmentedMatrix = (float**)malloc(size * sizeof(float*));
    for (i = 0; i < size; i++) {
    	augmentedMatrix[i] = (float*)malloc(size * sizeof(float));
    }

    float **inverseMatrix = (float**)malloc(size * sizeof(float*));
    for (i = 0; i < size; i++) {
    	inverseMatrix[i] = (float*)malloc(size * sizeof(float));
    }

    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
            augmentedMatrix[i][j] = matrix[i][j];
            inverseMatrix[i][j] = (i == j) ? 1.0f : 0.0f;
        }
    }

    for (i = 0; i < size; i++) {
        if (augmentedMatrix[i][i] == 0) {
            for (j = i + 1; j < size; j++) {
                if (augmentedMatrix[j][i] != 0) {
                    swapRows(augmentedMatrix, i, j, size);
                    swapRows(inverseMatrix, i, j, size);
                    break;
                }
            }
        }

        float scale = 1.0f / augmentedMatrix[i][i];
        scaleRow(augmentedMatrix, i, scale, size);
        scaleRow(inverseMatrix, i, scale, size);

        for (j = 0; j < size; j++) {
            if (j != i) {
                float factor = -augmentedMatrix[j][i];
                addRows(augmentedMatrix, j, i, factor, size);
                addRows(inverseMatrix, j, i, factor, size);
            }
        }
    }

    for (i = 0; i < size; i++) {
        free(augmentedMatrix[i]);
    }
    free(augmentedMatrix);

    return inverseMatrix;
}
float f1(float x, float y, float z, Seidel m) {
    return (m.result - (m.ycoef * y) - (m.zcoef * z)) / m.xcoef;
}

float f2(float x, float y, float z, Seidel m) {
    return (m.result - (m.xcoef * x) - (m.zcoef * z)) / m.ycoef;
}

float f3(float x, float y, float z, Seidel m) {
    return (m.result - (m.xcoef * x) - (m.ycoef * y)) / m.zcoef;
}

void ilerifark () {
	float h,result,x;
	parse();
	printf("lutfen h degerini giriniz: ");
	scanf(" %f",&h);
	getchar();
	printf("lutfen turevini almak istediginiz x noktasini giriniz: ");
	scanf(" %f",&x);
	getchar();
	result = (float) (-3 * fx(x) + 4 * fx(x + h) - fx(x + 2 * h)) / (2 * h);
	printf("\n x = %f noktasindaki ileri turev : %.4f",x,result);
}

void gerifark () {
	float h,result,x;
	parse();
	printf("lutfen h degerini giriniz: ");
	scanf(" %f",&h);
	getchar();
	printf("lutfen turevini almak istediginiz x noktasini giriniz: ");
	scanf(" %f",&x);
	getchar();
	result = (float) (-3 * fx(x) + 4 * fx(x - h) - fx(x - 2 * h)) / (-2 * h);
	printf("\n x = %f noktasindaki geri turev : %.4f",x,result);
}

void ortanokta () {
	float h,result,x;
	parse();
	printf("lutfen h degerini giriniz: ");
	scanf(" %f",&h);
	getchar();
	printf("lutfen turevini almak istediginiz x noktasini giriniz: ");
	scanf(" %f",&x);
	getchar();
	result = (float) (fx(x + h) - fx(x - h)) / (2 * h);
	printf("\n x = %f noktasindaki orta turev : %.4f",x,result);
}

void ikinciturev () {
	float h, result, x;
	parse();
	printf("lutfen h degerini giriniz: ");
	scanf(" %f",&h);
	getchar();
	printf("lutfen turevini almak istediginiz x noktasini giriniz: ");
	scanf(" %f",&x);
	getchar();
	result = (float) (fx(x - h) - 2 * fx(x) + fx(x + h)) / (pow(h,2));
	printf("\n x = %f noktasindaki 2. turev : %.4f",x,result);
}

void simpson1_3 () {
	float a, b, result;
	parse();
	printf("lutfen baslangic degerini giriniz: ");
	scanf(" %f",&a);
	getchar();
	printf("lutfen bitis degerini giriniz: ");
	scanf(" %f",&b);
	getchar();
	result = (float) (b - a) * (fx(a) + 4 * fx((a + b) / 2) + fx(b)) / 6;
	printf("simpson 1/3 kuralina gore intergral = %.4f ",result);
}

void simpson3_8 () {
	float a, b, result;
	parse();
	printf("lutfen baslangic degerini giriniz: ");
	scanf(" %f",&a);
	getchar();
	printf("lutfen bitis degerini giriniz: ");
	scanf(" %f",&b);
	getchar();
	result = (float) (b - a) * (fx(a) + 3 * fx(a + ((b - a) / 3)) + 3 * fx(a + (2 * (b - a) / 3)) + fx(b)) / 8;
	printf("simpson 3/8 kuralina gore intergral = %.4f ",result);
}

void Bisection() {//works
    float a, b, m, e;
    int flag = 0, i = 0, n;
    parse();
    printf("\nBISECTION YONTEMI ILE KOK BULMA\n");
    printf("Koku icerisine alan ilk degeri girin: ");
    fflush(stdin);
    if (scanf("%f", &a) != 1) {
        printf("Invalid input for 'a'.\n");
        return;
    }
    printf("Koku icerisine alan ikinci degeri girin: ");
    fflush(stdin);
    if (scanf("%f", &b) != 1) {
        printf("Invalid input for 'b'.\n");
        return;
    }
	printf("epsilon degerini giriniz: ");
	fflush(stdin);
    if (scanf("%f", &e) != 1) {
    	printf("Invalid input for 'e'.\n");
    	return;
	}
	printf("max iterasyon degerini giriniz: ");
	fflush(stdin);
    if (scanf("%d", &n) != 1) {
    	printf("Invalid input for 'n'.\n");
    	return;
	}
    m = (a + b) / 2;
    while (fabs(fx(m)) >= e && i < n) {
    	printf("\nstart    : %-10.6f \nend      : %-10.6f\nmid      : %-10.6f\nF(start) : %-10.6f\nF(end)   : %-10.6f\nF(mid)   : %-10.6f\niteration: %d\n",
        a, b, m, fx(a), fx(b), fx(m), i+1);
		i++;
        if (fx(a) * fx(b) < 0) {
            m = (a + b) / 2;
            if (fx(a) * fx(m) > 0) {
                a = m;
            } else {
                b = m;
            }
        } else {
            if (fx(a) * fx(b) == 0) {
                if (fx(a) == 0) {
                    m = a;
                } else {
                    m = b;
                }
            } else {
                flag = 1;
                printf("\nGirilen degerler koku icerisine almamaktadir.\n\n\n");
                e = fx(m) + 1;
            }
        }
    }
    if( i > n) {
		flag = 0;
	}
    if (flag == 0) {
        printf("\nDenklemin koku: %.5f\n\n\n", m);
    }
    return;
}

void RegulaFalsi(){//works
    float a, b, m, e;
    int flag = 0, i=0, n;
    parse();
    printf("\nREGULAFALSI YONTEMI ILE KOK BULMA\n");
    printf("Koku icerisine alan ilk degeri girin: ");
    fflush(stdin);
    if (scanf("%f", &a) != 1) {
        printf("Invalid input for 'a'.\n");
        return;
    }
    printf("Koku icerisine alan ikinci degeri girin: ");
    fflush(stdin);
    if (scanf("%f", &b) != 1) {
        printf("Invalid input for 'b'.\n");
        return;
    }
	printf("epsilon degerini giriniz: ");
	fflush(stdin);
    if (scanf("%f", &e) != 1) {
    	printf("Invalid input for 'e'.\n");
    	return;
	}
	printf("max iterasyon degerini giriniz: ");
	fflush(stdin);
    if (scanf("%d", &n) != 1) {
    	printf("Invalid input for 'n'.\n");
    	return;
	}
    
    m = (a * fx(b) - b * fx(a)) / (fx(b) - fx(a));
    printf("\nstart    : %-10.6f \nend      : %-10.6f\nmid      : %-10.6f\nF(start) : %-10.6f\nF(end)   : %-10.6f\nF(mid)   : %-10.6f\niteration: %d\n",
    a, b, m, fx(a), fx(b), fx(m), i+1);
    while(fabs(fx(m)) > e && i < n){
        if(fx(a) * fx(b) < 0){
            m = ((a * fx(b) - b * fx(a)) / (fx(b) - fx(a)));
            if(fx(a) * fx(m) > 0){
                a = m;
            }
            else{
                b = m;
            }
            i++;
            printf("\nstart    : %-10.6f \nend      : %-10.6f\nmid      : %-10.6f\nF(start) : %-10.6f\nF(end)   : %-10.6f\nF(mid)   : %-10.6f\niteration: %d\n",
        	a, b, m, fx(a), fx(b), fx(m), i+1);
        }
        else{
            if(fx(a) * fx(b) == 0){
                if(fx(a) == 0){
                    m = a;
                }
                else{
                    m = b;
                }
            }
            else{
                flag = 1;
                printf("\nGirilen degerler koku icerisine almamaktadir.");
                e = fx(m) + 1;
            }
        }
    }
	if( i > n) {
		flag = 0;
	}
    if(flag == 0){
        printf("\nDenklemin koku: %.5f\n\n\n", m);
    }
	return;
}

void NewtonRaphson () {//works
	float x0,x1,eps;
	int n, i = 0;
	printf("\nNEWTON RAPHSON YONTEMI ILE KOK BULMA\n");
	parse();
    printf("X0 degerini girin: ");
    scanf("%f",&x0);
	getchar();
    printf("Epsilon degerini girin: ");
    scanf(" %f",&eps);
	getchar();
	printf("Maximum iterasyon sayisi:");
	scanf(" %d",&n);
	getchar();
    x1 = x0 - (fx(x0) / turev(x0));
    while(fabs(x1 - x0) >= eps && i < n){
		printf("\nXn    : %10.6f \nXn+1  : %10.6f\nF(Xn) : %10.6f\nF'(Xn): %10.6f\niteration : %d\n",
        x0, x1, fx(x0), turev(x0), i+1);
        x0 = x1;
        x1 = x0 - (fx(x0) / turev(x0));
		i++;
    }
    printf("\nDenklemin koku: %.5f\n\n\n",x0);
	return;
}

void InverseMatrix() {//works
    int i, j, size;
	float d;
    printf("\nEnter the size of the square matrix: ");
    scanf("%d", &size);

    float **matrix = (float**)malloc(size * sizeof(float*));
    for (i = 0; i < size; i++) {
        matrix[i] = (float*)malloc(size * sizeof(float));
    }

    printf("\nEnter the elements of the matrix:\n");
    for (i = 0; i < size; i++) {
        for (j = 0; j < size; j++) {
			printf("[%d][%d]: ", i, j);
			fflush(stdin);
            scanf("%f", &matrix[i][j]);
        }
    }

	printf("\nEntered matrix:\n");
    for (i = 0; i < size; i++) {
        printf("\n  |");
        for (j = 0; j < size; j++) {
            printf("%10.4f", matrix[i][j]);
        }
        printf("       |");
    }
    float **invMatrix = inverse(matrix, size);
	d = determinant(matrix, size);
    if (d == 0) {
        printf("\n\nThe inverse does not exist.\n");
    } else {
        printf("\n\nInverse of the matrix:\n");
    	for (i = 0; i < size; i++) {
			printf("\n  |");
        	for (j = 0; j < size; j++) {
            	printf("%10.4f", invMatrix[i][j]);
        	}
        	printf("       |");
    	}
    }
	for (i = 0; i < size; i++) {
        free(matrix[i]);
    }
    free(matrix);
    for (i = 0; i < size; i++) {
        free(invMatrix[i]);
    }
    free(invMatrix);
}

void GaussElemination() {//works
    int n, i, j, k;
    float ratio;
    printf("\nGAUSS ELIMINATION METHODU ILE DENKLEM COZUMU\n");
    printf("Enter the number of equations: ");
    scanf("%d", &n);

    float *x = (float *)malloc(n * sizeof(float));

    float **a = (float **)malloc(n * sizeof(float *));
    for (i = 0; i < n; i++) {
        a[i] = (float *)malloc((n + 1) * sizeof(float));
    }

    printf("Enter the augmented matrix:\n");
    for (i = 0; i < n; i++) {
        for (j = 0; j <= n; j++) {
            printf("[%d][%d]: ", i, j);
            scanf("%f", &a[i][j]);
        }
    }

    for (i = 0; i < n - 1; i++) {
        if (a[i][i] == 0.0) {
            printf("Mathematical Error!");
            exit(0);
        }
        for (j = i + 1; j < n; j++) {
            ratio = a[j][i] / a[i][i];
            for (k = 0; k <= n; k++) {
                a[j][k] = a[j][k] - ratio * a[i][k];
            }
        }
    }

    x[n - 1] = a[n - 1][n] / a[n - 1][n - 1];

    for (i = n - 2; i >= 0; i--) {
        x[i] = a[i][n];
        for (j = i + 1; j < n; j++) {
            x[i] = x[i] - a[i][j] * x[j];
        }
        x[i] = x[i] / a[i][i];
    }

    printf("\nSolution:\n");
    for (i = 0; i < n; i++) {
        printf("x[%d] = %0.3f\n", i + 1, x[i]);
    }

    for (i = 0; i < n; i++) {
        free(a[i]);
    }
    free(a);
    free(x);
}

void GaussSeidel () {//works
	float x0 = 0, y0 = 0, z0 = 0, x1, y1, z1, e1, e2, e3, e, matrix[3][4];
 	int count = 1, i, j, k, flag, flag1 = 1;
 	Seidel x[3];
	printf("\nGAUSS SEIDEL YONTEMI ILE DENKLEM COZUMU\n");
 	printf("Matrixi giriniz:\n");

 	for(i = 0; i < 3; i++){
 		for(j = 0; j < 4; j++){
 			printf("[%d][%d]: ", i, j);
			fflush(stdin);
        	scanf("%f", &matrix[i][j]);
		}
	}
 	
 	printf("epsilon degerini giriniz:\n");
 	scanf(" %f", &e);
 	getchar();
 	
 	for (i = 0; i < 3; i++) {
    	flag = 1;
    	for (j = i + 1; j < 3; j++) {
        	if (fabs(matrix[i][i]) < fabs(matrix[j][i])) {
            	for (k = 0; k < 4; k++) {
                	float temp = matrix[i][k];
                	matrix[i][k] = matrix[j][k];
                	matrix[j][k] = temp;
            	}
            	flag = 0; // Set flag to 0 to indicate a swap occurred
            	break;    // Exit the inner loop once a swap is made
        	}
    	}
		if (flag == 0 && flag1 == 1){
			i = -1;
			flag1 = 0;
		}
	}

	for (i = 0; i < 3; i++) {
    	j = 0;
    	x[i].xcoef = matrix[i][j]; j++;
    	x[i].ycoef = matrix[i][j]; j++;
    	x[i].zcoef = matrix[i][j]; j++;
    	x[i].result = matrix[i][j];
	}	

 	printf("\nCount\tx\ty\tz\n");
 	do {
  		
  		x1 = f1(x0,y0,z0,x[0]);
  		y1 = f2(x1,y0,z0,x[1]);
  		z1 = f3(x1,y1,z0,x[2]);
  		printf("%d\t%0.4f\t%0.4f\t%0.4f\n",count, x1,y1,z1);

  		e1 = fabs(x0-x1);
  		e2 = fabs(y0-y1);
  		e3 = fabs(z0-z1);

  		count++;
  		
  		x0 = x1;
  		y0 = y1;
  		z0 = z1;
 	} while(e1>e && e2>e && e3>e);
 	
 	printf("\nSolution: x = %0.4f, y = %0.4f and z = %0.4f\n",x1,y1,z1);
}

void NumericalDifferentiation () {//works
	char x;
	printf("\nSAYISAL TUREV ILE TUREV BULMA\n");
	printf("Please choose one -> forward diff (f),backward diff (b),central diff (c),2nd diff (d):");
	scanf(" %c", &x);
	getchar();
	if (x == 'f') {
		ilerifark();
	} else if (x == 'b') {
		gerifark();
	} else if (x == 'c') {
		ortanokta();
	} else if (x == 'd') {
		ikinciturev();
	} else 
	printf("\nwrong input returning to menu:");
	return;
}
void SimpsonsRule () {//works
	char x;
	printf("\nSIMPSON'S RULE ILE INTEGRAL BULMA\n");
	printf("lutfen bir tanesini secin:\n simpson 1/3 kurali (0):\n simpson 3/8 kurali (1):\n ");
	scanf(" %c",&x);
	getchar();
	if(x == '0'){
		simpson1_3();
	} else if(x == '1') {
		simpson3_8();
	} else 
	printf("\n invalid response returning to menu:");
	return;
}
void TrapezoidalRule () { //works
	float a, b, result;
	printf("\nTRAPEZOIDAL RULE ILE INTEGRAL BULMA\n");
	parse();
	printf("lutfen baslangic degerini giriniz: ");
	scanf(" %f",&a);
	getchar();
	printf("lutfen bitis degerini giriniz: ");
	scanf(" %f",&b);
	getchar();
	result = (b - a) * ((fx(a) + fx(b)) / 2);
	printf("\nYamuk kuralina gore intergral = %.4f ",result);
}
void GregoryNewton () { //works
	float x, x1, x2, y, y1, y2, x3, y3, value, a, a1, a2, b, b1, c, result;
	printf("\nGREGORY NEWTON YONTEMI ILE DEGER BULMA\n");
	printf("\nlutfen ilk noktanin x kordinatini girin :");
	scanf(" %f",&x);
	getchar();
	printf("\nlutfen ilk noktanin y kordinatini girin :");
	scanf(" %f",&y);
	getchar();
	printf("\nlutfen ikinci noktanin x kordinatini girin :");
	scanf(" %f",&x1);
	getchar();
	printf("\nlutfen ikinci noktanin y kordinatini girin :");
	scanf(" %f",&y1);
	getchar();
	printf("\nlutfen ucuncu noktanin x kordinatini girin :");
	scanf(" %f",&x2);
	getchar();
	printf("\nlutfen ucuncu noktanin y kordinatini girin :");
	scanf(" %f",&y2);
	getchar();
	printf("\nlutfen dorduncu noktanin x kordinatini girin :");
	scanf(" %f",&x3);
	getchar();
	printf("\nlutfen dorduncu noktanin y kordinatini girin :");
	scanf(" %f",&y3);
	getchar();
	printf("\n| x |  =  |  %f  |  %f  |  %f  |  %f  |\n\n|F(x)| =  |  %f  |  %f  |  %f  |  %f  |", x, x1, x2, x3, y, y1, y2, y3);
	a = (y1 - y) / (x1 - x);
	a1 = (y2 - y1) / (x2 - x1);
	a2 = (y3 - y2) / (x3 - x2);
	b = (a1 - a) / (x2 - x);
	b1 = (a2 - a1) / (x3 - x1);
	c = (b1 - b) / (x3 - x);
	printf("\n\n %.2f + %.2f * (x - %.2f) + %.2f * (x - %.2f) * (x - %.2f) + %.2f * (x - %.2f) * (x - %.2f) * (x - %.2f)", y, a, x, b, x, x1, c, x, x1, x2);
	printf("\n\nlutfen degerini hesaplamak istediginiz noktayi girin :");
	scanf(" %f",&value);
	getchar();
	result = y + a * (value - x) + b * (value - x) * (value - x1) + c * (value - x) * (value - x1) * (value - x2);
	printf("\ndegerimiz = %.4f",result);
}
