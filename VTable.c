// Ryan Suos
// CECS 342
// Friday 1:00pm
// Project 3

#include <stdio.h>
#include <stdlib.h>

struct Employee
{
    void** vPointer;
    int age;
};

struct HourlyEmployee
{
    void** vPointer;
    int age;
    double hourly_rate;
    double hours;
};

struct CommissionEmployee
{
    void** vPointer;
    int age;
    double sales_amount;
};

struct SeniorSalesman
{
    void** vPointer;
    int age;
    double sales_amount;
};

void Speak_Hourly(struct HourlyEmployee* he)
{
    printf("I'm %d years old. I work %f hours a week for $%.2f dollars per hour.", he->age, he->hours, he->hourly_rate);
}

double GetPay_Hourly(struct HourlyEmployee* he)
{
    return (he->hourly_rate * he->hours);
}

double GetHours_Hourly(struct HourlyEmployee* he)
{
    return he->hours;
}

void Speak_Commission(struct CommissionEmployee* ce)
{
    printf("I am %d years old and I made commission on $%.2f dollars in sales!", ce->age, ce->sales_amount);
}

double GetPay_Commission(struct CommissionEmployee* ce)
{
    double total = ((ce->sales_amount * .1) + 40000);
    return total;
}

// Don't need this
//void Speak_Senior(struct SeniorSalesman* ss)
//{
//    printf("I make commission on %f dollars in sales!", ss->sales_amount);
//}

double GetPay_Senior(struct SeniorSalesman* ss)
{
    if (ss->age < 40) return ((ss->sales_amount * .2) + 50000);
    else return ((ss->sales_amount * .2) + 50000 + (ss->sales_amount *.05));
}

void Construct_Hourly(struct HourlyEmployee* he)
{
    he->age = 0;
    he->hourly_rate = 0;
    he->hours = 0;
}

void Construct_Commission(struct CommissionEmployee* ce)
{
    ce->age = 0;
    ce->sales_amount = 0;
}

void Construct_Senior(struct SeniorSalesman* ss)
{
    ss->age = 0;
    ss->sales_amount = 0;
}

void* Vtable_Hourly[2] = {Speak_Hourly, GetPay_Hourly};
void* Vtable_Commission[2] = {Speak_Commission, GetPay_Commission};
void* Vtable_Senior[2] = {Speak_Commission ,GetPay_Senior};

int main(int argc, const char * argv[]) {
    struct Employee* e;
    
    int choice;
    int age1;

    //Vtable_Hourly[0]((struct HourlyEmployee *)&h);
    //((void (*)(struct Employee*))Vtable_Hourly[0])((struct Employee *)&h);
    
    printf("Choose from the following:\n1)Hourly employee\n2)Commission employee\n3)Senior salesman.\n");
    
    scanf("%d", &choice);
    
    printf("\nWhat is the age of the employee? ");
    
    scanf("%d", &age1);
    
    
    switch (choice)
    {
        case 1:
            printf("How many hours does the employee work? ");
            
            double hours1;
            double rate;

            scanf("%lf", &hours1);
            
            printf("What is the employee's hourly rate? ");
            scanf("%lf", &rate);
            
            struct HourlyEmployee* h = malloc(sizeof (struct HourlyEmployee));
            Construct_Hourly(h);
            h->hours = hours1;
            h->hourly_rate = rate;
            h->age = age1;
            
            e = h;
            
            printf("\n\nEmployee will now speak:\n");
            ((void (*)(struct Employee*))Vtable_Hourly[0])((struct Employee *)e);
            
            printf("\nMakes: %.2f", ((double (*)(struct Employee*))Vtable_Hourly[1])((struct Employee *)e));
            ((void (*)(struct Employee*))Vtable_Hourly[1])((struct Employee *)e);
            
            break;
        case 2:
            printf("\nWhat are the employees sales amount?");
            double sales;
            
            scanf("%lf", &sales);
            
            struct CommissionEmployee *c = malloc(sizeof (struct CommissionEmployee));
            Construct_Commission(c);
            c->sales_amount = sales;
            c->age = age1;
            
            e = c;
            
            printf("\n\nEmployee will now speak:\n");
            ((void (*)(struct Employee*))Vtable_Commission[0])((struct Employee *)e);
            
            printf("\nMakes: %.2f", ((double (*)(struct Employee*))Vtable_Commission[1])((struct Employee *)e));
            ((void (*)(struct Employee*))Vtable_Hourly[1])((struct Employee *)e);
            break;
        case 3:
            printf("What are the employees sales amount?");
            double sales1;
            
            scanf("%lf", &sales1);
            
            struct SeniorSalesman *s = malloc(sizeof (struct SeniorSalesman));
            Construct_Senior(s);
            s->sales_amount = sales1;
            s->age = age1;
            
            e = s;
            
            printf("\n\nEmployee will now speak:\n");
            ((void (*)(struct Employee*))Vtable_Senior[0])((struct Employee *)e);
            
            printf("\nMakes: %.2f", ((double (*)(struct Employee*))Vtable_Senior[1])((struct Employee *)e));
            ((void (*)(struct Employee*))Vtable_Hourly[1])((struct Employee *)e);
            break;
        default:
            printf("Choose an appropriate response.");
            break;
    }
    
}

