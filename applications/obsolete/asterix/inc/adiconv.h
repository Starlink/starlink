#if !defined(_ADI_CONV_H_)

#define _ADI_CONV_H_ 1

typedef
  void (*ADIconvertor)(ADImta *,int,char *,ADImta *,char *,int *,ADIstatus);


#ifdef __cplusplus
extern "C" {
#endif

ADIconvertor 	ADIcnvFind( ADIclassDef *from, ADIclassDef *to,
			ADIstatus status );
void		ADIcnvInit( ADIstatus status );
void 		ADIcnvNew( ADIclassDef *from, ADIclassDef *to,
			ADIconvertor func, ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif

