export function rowsFromRecords(kind, records) {
  return records.filter(row => row.kind === 'sample').map(row => {
    switch (kind) {
      case 'poisson': return [row.count, row.frequency];
      case 'regression': return [row.draw, row.f0, row.f6, ...row.predictive];
      case 'gp': case 'wiener': case 'piecewise': return [row.draw, ...row.values];
      case 'clustering': return [row.point, row.x, row.y, row.cluster, row.centre_x, row.centre_y, row.sigma];
      case 'irm': return [row.person, ...row.probabilities];
      case 'program': return [row.program, row.x, row.y];
      case 'ibp': return [row.country, ...row.features];
      case 'graph': return [row.vertex, ...row.edges];
      case 'mondrian': return [row.block, row.x_lower, row.x_upper, row.y_lower, row.y_upper, row.value];
      default: throw new Error(`Unknown model: ${kind}`);
    }
  });
}
