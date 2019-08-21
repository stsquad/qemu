/*
 * TB statistics logging flags
 *
 * These are separate from the main tb-stats.h as they are visible to the common code.
 *
 * Copyright (c) 2019
 * Written by Alex Bennée <alex.bennee@linaro.org>
 *
 * This code is licensed under the GNU GPL v2, or later.
 */

#ifndef _TB_STATS_FLAGS_H_
#define _TB_STATS_FLAGS_H_

/* TBStatistic collection controls */
enum TBStatsStatus {
    TB_STATS_DISABLED = 0,
    TB_STATS_RUNNING,
    TB_STATS_PAUSED,
    TB_STATS_STOPPED
};

#define TB_NOTHING    (1 << 0)
#define TB_EXEC_STATS (1 << 1)
#define TB_JIT_STATS  (1 << 2)
#define TB_JIT_TIME   (1 << 3)

extern int tcg_collect_tb_stats;
extern uint32_t default_tbstats_flag;

void enable_collect_tb_stats(void);
void disable_collect_tb_stats(void);
void pause_collect_tb_stats(void);
bool tb_stats_collection_enabled(void);
bool tb_stats_collection_paused(void);

uint32_t get_default_tbstats_flag(void);

#endif /* _TB_STATS_FLAGS_H_ */
