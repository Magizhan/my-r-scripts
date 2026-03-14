/**
 * Claude Usage Leaderboard - Cloudflare Worker
 *
 * Serves the dashboard and handles API endpoints for the bookmarklet.
 * Data is stored in Cloudflare KV.
 *
 * KV Keys:
 *   users       -> JSON array of { id, name, team, numPlans }
 *   usage:{id}  -> JSON { amount, pct, timestamp, source }
 *   config      -> JSON { planCost }
 */

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    const path = url.pathname;

    // CORS headers for bookmarklet cross-origin requests
    const corsHeaders = {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    };

    if (request.method === 'OPTIONS') {
      return new Response(null, { headers: corsHeaders });
    }

    try {
      // API Routes
      if (path.startsWith('/api/')) {
        const response = await handleApi(path, request, env);
        // Add CORS headers to all API responses
        const newHeaders = new Headers(response.headers);
        Object.entries(corsHeaders).forEach(([k, v]) => newHeaders.set(k, v));
        return new Response(response.body, {
          status: response.status,
          headers: newHeaders,
        });
      }

      // Serve static files from KV (site bucket)
      return env.ASSETS
        ? env.ASSETS.fetch(request)
        : new Response('Dashboard not found. Deploy static assets.', { status: 404 });
    } catch (err) {
      return jsonResponse({ error: err.message }, 500, corsHeaders);
    }
  },
};

async function handleApi(path, request, env) {
  const method = request.method;

  // GET /api/data - Full leaderboard data
  if (path === '/api/data' && method === 'GET') {
    return getLeaderboardData(env);
  }

  // GET /api/users - List users
  if (path === '/api/users' && method === 'GET') {
    return getUsers(env);
  }

  // POST /api/users - Add user
  if (path === '/api/users' && method === 'POST') {
    return addUser(await request.json(), env);
  }

  // DELETE /api/users/:id
  if (path.startsWith('/api/users/') && method === 'DELETE') {
    const id = path.split('/api/users/')[1];
    return deleteUser(id, env);
  }

  // POST /api/usage - Log usage (from bookmarklet or manual)
  if (path === '/api/usage' && method === 'POST') {
    return logUsage(await request.json(), env);
  }

  // POST /api/users/:id/plans - Add plans
  if (path.match(/^\/api\/users\/[^/]+\/plans$/) && method === 'POST') {
    const id = path.split('/')[3];
    return addPlans(id, await request.json(), env);
  }

  // POST /api/import - Import full dataset
  if (path === '/api/import' && method === 'POST') {
    return importData(await request.json(), env);
  }

  // GET /api/export - Export full dataset
  if (path === '/api/export' && method === 'GET') {
    return exportData(env);
  }

  return jsonResponse({ error: 'Not found' }, 404);
}

// ============================================================
// DATA ACCESS
// ============================================================

async function getUsers(env) {
  const users = await kvGet(env, 'users', []);
  return jsonResponse(users);
}

async function addUser(body, env) {
  const { name, team, numPlans = 1 } = body;
  if (!name || !team) return jsonResponse({ error: 'name and team required' }, 400);

  const users = await kvGet(env, 'users', []);
  const id = 'u_' + Date.now().toString(36) + '_' + Math.random().toString(36).slice(2, 7);
  users.push({ id, name, team, numPlans: parseInt(numPlans) || 1 });
  await kvPut(env, 'users', users);
  return jsonResponse({ id, name, team, numPlans });
}

async function deleteUser(id, env) {
  let users = await kvGet(env, 'users', []);
  const user = users.find(u => u.id === id);
  if (!user) return jsonResponse({ error: 'User not found' }, 404);

  users = users.filter(u => u.id !== id);
  await kvPut(env, 'users', users);
  await env.LEADERBOARD_KV.delete(`usage:${id}`);
  return jsonResponse({ ok: true, removed: user.name });
}

async function logUsage(body, env) {
  const { userId, name, pct, amount, source = 'manual' } = body;

  // Find user by ID or name
  const users = await kvGet(env, 'users', []);
  let user;
  if (userId) {
    user = users.find(u => u.id === userId);
  } else if (name) {
    user = users.find(u => u.name.toLowerCase() === name.toLowerCase());
  }

  if (!user) return jsonResponse({ error: 'User not found. Add user first.' }, 404);

  const planCost = parseInt(env.PLAN_COST || '200');
  let usagePct, usageAmount;

  if (pct !== undefined) {
    usagePct = parseFloat(pct);
    usageAmount = (usagePct / 100) * user.numPlans * planCost;
  } else if (amount !== undefined) {
    usageAmount = parseFloat(amount);
    usagePct = (usageAmount / (user.numPlans * planCost)) * 100;
  } else {
    return jsonResponse({ error: 'pct or amount required' }, 400);
  }

  const usageData = {
    userId: user.id,
    pct: usagePct,
    amount: usageAmount,
    timestamp: new Date().toISOString(),
    source,
  };

  await kvPut(env, `usage:${user.id}`, usageData);
  return jsonResponse({ ok: true, user: user.name, ...usageData });
}

async function addPlans(id, body, env) {
  const users = await kvGet(env, 'users', []);
  const user = users.find(u => u.id === id);
  if (!user) return jsonResponse({ error: 'User not found' }, 404);

  const count = parseInt(body.count) || 1;
  user.numPlans += count;
  await kvPut(env, 'users', users);
  return jsonResponse({ ok: true, name: user.name, numPlans: user.numPlans });
}

async function getLeaderboardData(env) {
  const users = await kvGet(env, 'users', []);
  const planCost = parseInt(env.PLAN_COST || '200');

  const board = await Promise.all(users.map(async (u) => {
    const usage = await kvGet(env, `usage:${u.id}`, null);
    const budget = u.numPlans * planCost;
    const used = usage ? usage.amount : 0;
    const pct = usage ? usage.pct : 0;
    return {
      ...u,
      budget,
      used,
      pct,
      lastUpdated: usage ? usage.timestamp : null,
      source: usage ? usage.source : null,
    };
  }));

  board.sort((a, b) => b.pct - a.pct);

  // Team stats
  const nyUsers = board.filter(u => u.team === 'NY');
  const xyneUsers = board.filter(u => u.team === 'Xyne');

  return jsonResponse({
    users: board,
    stats: {
      totalUsers: board.length,
      totalBudget: board.reduce((s, u) => s + u.budget, 0),
      totalSpend: board.reduce((s, u) => s + u.used, 0),
      avgPct: board.length > 0 ? board.reduce((s, u) => s + u.pct, 0) / board.length : 0,
    },
    teams: {
      NY: {
        members: nyUsers.length,
        avgPct: nyUsers.length > 0 ? nyUsers.reduce((s, u) => s + u.pct, 0) / nyUsers.length : 0,
        totalSpend: nyUsers.reduce((s, u) => s + u.used, 0),
      },
      Xyne: {
        members: xyneUsers.length,
        avgPct: xyneUsers.length > 0 ? xyneUsers.reduce((s, u) => s + u.pct, 0) / xyneUsers.length : 0,
        totalSpend: xyneUsers.reduce((s, u) => s + u.used, 0),
      },
    },
    updatedAt: new Date().toISOString(),
  });
}

async function importData(body, env) {
  const { users = [], usageLogs = [] } = body;
  await kvPut(env, 'users', users);
  for (const log of usageLogs) {
    if (log.userId) {
      await kvPut(env, `usage:${log.userId}`, log);
    }
  }
  return jsonResponse({ ok: true, imported: users.length });
}

async function exportData(env) {
  const users = await kvGet(env, 'users', []);
  const usageLogs = [];
  for (const u of users) {
    const usage = await kvGet(env, `usage:${u.id}`, null);
    if (usage) usageLogs.push(usage);
  }
  return jsonResponse({ users, usageLogs });
}

// ============================================================
// HELPERS
// ============================================================

async function kvGet(env, key, defaultVal) {
  const val = await env.LEADERBOARD_KV.get(key, 'json');
  return val !== null ? val : defaultVal;
}

async function kvPut(env, key, val) {
  await env.LEADERBOARD_KV.put(key, JSON.stringify(val));
}

function jsonResponse(data, status = 200, extraHeaders = {}) {
  return new Response(JSON.stringify(data), {
    status,
    headers: { 'Content-Type': 'application/json', ...extraHeaders },
  });
}
