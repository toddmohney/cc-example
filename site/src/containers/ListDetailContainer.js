import { connect } from 'react-redux';
import { selectConsumer } from '../actions';
import ListDetailView from '../components/ListDetailView';

const mapStateToProps = (state) => {
  return {
    listData: state.meatbarEaters,
    selectedListItem: state.selectedEater
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    onListItemClick: (id) => {
      dispatch(selectConsumer(id));
    }
  };
};

const ListDetailContainer = connect(
  mapStateToProps,
  mapDispatchToProps
)(ListDetailView);

export default ListDetailContainer;
