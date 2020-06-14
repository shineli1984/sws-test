import React from 'react'
import {
  filterScore,
  selectLoading,
  selectExchanges,
  filterExchanges,
  sort,
  sortDirection,
  selectFilters,
  selectSortBy,
  selectLoadingError
} from './companiesSlice';
import { useSelector, useDispatch } from 'react-redux';

const Filters = () => {
  const loading = useSelector(selectLoading);
  const exchanges = useSelector(selectExchanges);
  const filters = useSelector(selectFilters);
  const sortBy = useSelector(selectSortBy);
  const loadingError = useSelector(selectLoadingError);
  const dispatch = useDispatch();
  const dispatchSort = field => dispatch(sort({field}));
  const dispatchsortDirection = desc => dispatch(sortDirection({desc}));
  
  // TODO: add debounce
  return <div>
    {
      loading ? <div>Loading...</div> : null
    }
    {
      loadingError ? <div>{loadingError}</div> : null
    }
    <label>
      Above Score:&nbsp;
      <input 
        value={filters.score}
        type="number"
        onChange={e => dispatch(filterScore({score: e.target.value}))} />
    </label>
    <br/>
    <label>
      On Exchanges:&nbsp;
      <select 
        multiple={true} 
        onChange={e => dispatch(
            filterExchanges({
              values: [...e.target.options]
                .filter(o => o.selected)
                .map(o => o.value)
            })
          )}
      >
        {
          exchanges.map(exchange => 
            <option 
              value={exchange} 
              key={exchange}
              selected={filters.exchanges.find(e => e === exchange)}
            >
                {exchange}
            </option>
          )
        }
      </select>
    </label>
    <div>
      Sort by:&nbsp;
      <label><input type="radio" name="by" onChange={_e => dispatchSort("variance")} checked={sortBy.field === "variance"}/>Volatility</label>
      <label><input type="radio" name="by" onChange={_e => dispatchSort("score")} checked={sortBy.field === "score"} />Score</label>
    </div>
    <div>
      Direction:&nbsp;
      <label><input type="radio" name="direction" disabled={sortBy.field === null} checked={sortBy.desc === 1} onChange={_e => dispatchsortDirection(true)} />Descending</label>
      <label><input type="radio" name="direction" disabled={sortBy.field === null} checked={sortBy.desc === 0} onChange={_e => dispatchsortDirection(false)} />Ascending</label>
    </div>
  </div>
}
  

export default Filters