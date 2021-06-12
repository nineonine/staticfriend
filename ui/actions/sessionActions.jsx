import api from '../helpers/api';
import {
    RUN_SESSION,
    RUN_SESSION_FAILED,
    RUN_SESSION_START
} from './types';

export const runSession = (source_in, source_out, optimization, program) => {
    return dispatch => {
        dispatch({type:RUN_SESSION_START});
        api.post('/session')
        .then((response) => {
            if (response.ok) {
                console.log(response.data);
                dispatch({type:RUN_SESSION, data: response.data});
            } else {
                let err = response.status === 404 ? 'not found' : response.problem;
                dispatch({type:RUN_SESSION_FAILED, error: err});
            }
        })
    }
}
