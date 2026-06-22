#include <stdint.h>
#include "Random123/philox.h"

/* -------------------------------------------------------------------------- */

/*  C storage: out[10][2]                                                      */
/* -------------------------------------------------------------------------- */
void philox2x32_10_trace_ref(const int32_t ctr[2], const int32_t key[1], int32_t out[10][2])
{
    philox2x32_ctr_t c = {{ (uint32_t)ctr[0], (uint32_t)ctr[1] }};
    philox2x32_key_t k = {{ (uint32_t)key[0] }};

    const uint32_t W0 = 0x9E3779B9u;

    for (int i = 0; i < 10; ++i) {
        c = _philox2x32round(c, k);
        out[i][0] = (int32_t)c.v[0];
        out[i][1] = (int32_t)c.v[1];
        if (i < 9) {
            k.v[0] += W0;
        }
    }
}

/* -------------------------------------------------------------------------- */
/*  4x32 trace                                                                 */
/*  C storage: out[10][4]                                                      */
/* -------------------------------------------------------------------------- */
void philox4x32_10_trace_ref(const int32_t ctr[4], const int32_t key[2], int32_t out[10][4])
{
    philox4x32_ctr_t c = {{
        (uint32_t)ctr[0], (uint32_t)ctr[1],
        (uint32_t)ctr[2], (uint32_t)ctr[3]
    }};
    philox4x32_key_t k = {{ (uint32_t)key[0], (uint32_t)key[1] }};

    const uint32_t W0 = 0x9E3779B9u;
    const uint32_t W1 = 0xBB67AE85u;

    for (int i = 0; i < 10; ++i) {
        c = _philox4x32round(c, k);
        out[i][0] = (int32_t)c.v[0];
        out[i][1] = (int32_t)c.v[1];
        out[i][2] = (int32_t)c.v[2];
        out[i][3] = (int32_t)c.v[3];
        if (i < 9) {
            k.v[0] += W0;
            k.v[1] += W1;
        }
    }
}

/* -------------------------------------------------------------------------- */
/*  2x64 trace                                                                 */
/*  C storage: out[10][2]                                                      */
/* -------------------------------------------------------------------------- */
void philox2x64_10_trace_ref(const int64_t ctr[2], const int64_t key[1], int64_t out[10][2])
{
    philox2x64_ctr_t c = {{ (uint64_t)ctr[0], (uint64_t)ctr[1] }};
    philox2x64_key_t k = {{ (uint64_t)key[0] }};

    const uint64_t W0 = UINT64_C(0x9E3779B97F4A7C15);

    for (int i = 0; i < 10; ++i) {
        c = _philox2x64round(c, k);
        out[i][0] = (int64_t)c.v[0];
        out[i][1] = (int64_t)c.v[1];
        if (i < 9) {
            k.v[0] += W0;
        }
    }
}

/* -------------------------------------------------------------------------- */
/*  4x64 trace                                                                 */
/*  C storage: out[10][4]                                                      */
/* -------------------------------------------------------------------------- */
void philox4x64_10_trace_ref(const int64_t ctr[4], const int64_t key[2], int64_t out[10][4])
{
    philox4x64_ctr_t c = {{
        (uint64_t)ctr[0], (uint64_t)ctr[1],
        (uint64_t)ctr[2], (uint64_t)ctr[3]
    }};
    philox4x64_key_t k = {{ (uint64_t)key[0], (uint64_t)key[1] }};

    const uint64_t W0 = UINT64_C(0x9E3779B97F4A7C15);
    const uint64_t W1 = UINT64_C(0xBB67AE8584CAA73B);

    for (int i = 0; i < 10; ++i) {
        c = _philox4x64round(c, k);
        out[i][0] = (int64_t)c.v[0];
        out[i][1] = (int64_t)c.v[1];
        out[i][2] = (int64_t)c.v[2];
        out[i][3] = (int64_t)c.v[3];
        if (i < 9) {
            k.v[0] += W0;
            k.v[1] += W1;
        }
    }
}
