#ifndef RT_NAMES_H_
#define RT_NAMES_H_ 1

extern const char* rtnl_rtrealm_n2a(int id, char *buf, int len);
extern int rtnl_rtrealm_a2n(u_int32_t *id, const char *arg);

#endif
