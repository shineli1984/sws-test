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
import TextField from '@material-ui/core/TextField';
import { useSelector, useDispatch } from 'react-redux';
import Autocomplete from '@material-ui/lab/Autocomplete';
import Radio from '@material-ui/core/Radio';
import FormControl from '@material-ui/core/FormControl';
import FormLabel from '@material-ui/core/FormLabel';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import Switch from '@material-ui/core/Switch';
import Grid from '@material-ui/core/Grid';
import Alert from '@material-ui/lab/Alert';
import { makeStyles } from '@material-ui/core/styles';

const useStyles = makeStyles({
  root: {
    padding: "1rem"
  }
});


const Filters = () => {
  const exchanges = useSelector(selectExchanges);
  const filters = useSelector(selectFilters);
  const sortBy = useSelector(selectSortBy);
  const loadingError = useSelector(selectLoadingError);
  const dispatch = useDispatch();
  const dispatchSort = field => dispatch(sort({field}));
  const dispatchsortDirection = desc => dispatch(sortDirection({desc}));
  const classes = useStyles()

  return <FormControl component="fieldset" className={classes.root}>
    <Grid container spacing={3}>
      {
        loadingError ? <Grid xs={12}><Alert severity="error">{loadingError}</Alert></Grid> : null
      }
      <Grid item xs={12} sm={6}>
        <Autocomplete
          multiple
          value={filters.exchanges}
          filterSelectedOptions
          options={exchanges}
          getOptionLabel={option => option}
          onChange={(_e, values) => dispatch(
            filterExchanges({
              values
            })
          )}
          renderInput={(params) => (
            <TextField
              {...params}
              variant="standard"
              label="Exhanges"
              placeholder="Filter by exchanges"
            />
          )}
        />
      </Grid>
      <Grid item xs={12} sm={6}>
        <TextField 
          label="Above Score:&nbsp;"
          value={filters.score}
          type="number"
          onChange={e => dispatch(filterScore({score: e.target.value}))} />
      </Grid>
      <Grid item xs={6}>
        <FormLabel component="legend">Sort by</FormLabel>
        <FormControlLabel name="sortBy" checked={sortBy.field === "variance"} value="variance" control={<Radio />} label="Volatility" onChange={(_e, value) => dispatchSort("variance")} />
        <FormControlLabel name="sortBy" checked={sortBy.field === "score"} value="score" control={<Radio />} label="Score" onChange={(_e, value) => dispatchSort("score")} />
      
        <FormControlLabel
          control={
            <Switch
              disabled={sortBy.field === null}
              checked={sortBy.desc === 1}
              onChange={(_e, value) => dispatchsortDirection(value)}
              name="Descending"
              inputProps={{ 'aria-label': 'secondary checkbox' }}
            />
          }
          label="Descending"
        />
      </Grid>
    </Grid>
  </FormControl>
}
  

export default Filters